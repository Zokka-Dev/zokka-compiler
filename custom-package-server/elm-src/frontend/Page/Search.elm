port module Page.Search exposing
  ( Model
  , init
  , Msg
  , update
  , view
  )


import Browser.Navigation as Nav
import Dict exposing (Dict)
import Elm.Version as V exposing (Version)
import Html exposing (..)
import Html.Attributes exposing (autocomplete, autofocus, class, href, name, placeholder, style, type_, value)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Html.Keyed as Keyed
import Http
import Href
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Page.Search.Entry as Entry exposing (AuthToken, DataFromBackend, Package, Repository, authTokenDecoder, newlyCreatedAuthTokenDecoder)
import Page.Problem as Problem
import Process
import Session
import Skeleton
import Task
import Url.Builder as Url
import Url
import Utils.AuthTokenId as AuthTokenId exposing (AuthTokenId)
import Utils.DictUtils exposing (groupBy)
import Utils.LoginUpdate exposing (LoginUpdate(..), httpErrorToLoginUpdate)
import Utils.NonEmptyList as NonEmptyList
import Utils.PermissionLevel as PermissionLevel exposing (PermissionLevel)
import Utils.RepositoryId as RepositoryId exposing (RepositoryId(..))
import Utils.RepositoryName as RepositoryName exposing (RepositoryName)
import Utils.RepositoryUrlSafeName as RepositoryUrlSafeName exposing (RepositoryUrlSafeName)



-- MODEL


type alias Model =
  { session : Session.Data
  , query : String
  -- Pulling out the repository and author separately are for cases where we want to display a view for /repository/author URLs
  --
  -- FIXME: This is technically bad data modelling; it shouldn't be possible for
  -- the author to be Just ... and the repository to be Nothing
  , repository : Maybe String
  , author : Maybe String
  , entries : Entries
  -- These two are fields to handle forms that need text input to create a repository
  , createRepositoryCurrentName : String
  , createRepositoryCurrentUrlSafeName : String
  }


type Entries
  = Failure
  | Loading
  | Success (List Repository)



init : Session.Data -> Maybe String -> Maybe String -> Maybe String -> ( Model, Cmd Msg, LoginUpdate )
init session query repository author =
  case session.entries of
    Just backendData ->
      ( Model
        session
        (Maybe.withDefault "" query)
        repository
        author
        (Success (Entry.backendDataToRepositories backendData |> Dict.values))
        ""
        ""
      , Cmd.none
      , NoUpdateAboutLoginStatus
      )

    Nothing ->
      ( Model
        session
        (Maybe.withDefault "" query)
        repository
        author
        Loading
        ""
        ""
      , Http.send GotPackages <|
          Http.get "/dashboard/all-data" (Entry.dashboardDataDecoder)
      , NoUpdateAboutLoginStatus
      )



-- UPDATE


type Msg
  = QueryChanged String
  | QuerySubmitted
  | GotPackages (Result Http.Error DataFromBackend)
  | OpenGenericDialogById String
  | CloseGenericDialogById String
  -- We special case this one because we need to do something other than just close the dialog when we get this
  | CloseCreateTokenDialog RepositoryId
  | CreateRepositoryRequestRepositoryName String
  | CreateRepositoryRequestRepositoryUrlSafeName String
  | SendCreateRepositoryRequest
  | ReceivedCreateRepositoryConfirmation (Result Http.Error RepositoryId)
  | SendDeleteTokenRequest RepositoryId AuthTokenId
  | ReceivedDeleteTokenConfirmation (Result Http.Error (RepositoryId, AuthTokenId))
  | SendCreateTokenRequest RepositoryId PermissionLevel
  | ReceivedCreateTokenConfirmation (Result Http.Error (AuthToken, String))
  | SendDeleteRepositoryRequest RepositoryId
  | ReceivedDeleteRepositoryConfirmation (RepositoryId, Result Http.Error ())
  | SendDeletePackageRequest PackageIdentifier
  | ReceivedDeletePackageConfirmation (PackageIdentifier, Result Http.Error ())
  | DelayedUrlChange String


type alias PackageIdentifier = { repository: RepositoryId, author: String, project: String, version: String }


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg, LoginUpdate )
update key msg model =
  case msg of
    QueryChanged query ->
      ( { model | query = query }
      , Process.sleep 300
          |> Task.perform (\_ -> DelayedUrlChange query)
      , NoUpdateAboutLoginStatus
      )

    GotPackages result ->
      case Debug.log "GotPackages result" result of
        Err httpError ->
            ({ model | entries = Failure }, Cmd.none, httpErrorToLoginUpdate httpError)

        Ok backendData ->
          ( { model
                | entries = Success (Entry.backendDataToRepositories backendData |> Dict.values)
                , session = Session.addEntries backendData model.session
            }
          , Cmd.none
          , ConfirmedUserIsLoggedIn
          )

    DelayedUrlChange query ->
      if query == model.query then
        ( model, addQueryToUrl key model, NoUpdateAboutLoginStatus )
      else
        -- Input changed, do nothing
        ( model, Cmd.none, NoUpdateAboutLoginStatus )

    QuerySubmitted ->
      if String.contains "->" model.query then
        ( model, searchByType model.query, NoUpdateAboutLoginStatus )
      else
        ( model, Cmd.none, NoUpdateAboutLoginStatus )

    OpenGenericDialogById dialogId ->
      ( model
      , openDialogById dialogId
      , NoUpdateAboutLoginStatus )

    CloseGenericDialogById dialogId ->
      ( model
      , closeDialogById dialogId
      , NoUpdateAboutLoginStatus )

    SendDeleteTokenRequest repository authTokenId ->
      ( model
      , Http.send ReceivedDeleteTokenConfirmation <| httpDelete
        ("/dashboard/repository/" ++ (RepositoryId.toString repository) ++ "/token/" ++ (AuthTokenId.toString authTokenId))
        (repository, authTokenId)
      , NoUpdateAboutLoginStatus
      )

    ReceivedDeleteTokenConfirmation result ->
      case Debug.log "ReceivedDeleteTokenConfirmation result" result of
        Err httpError ->
            ({ model | entries = Failure }, Cmd.none, httpErrorToLoginUpdate httpError)

        Ok (repository, authTokenId) ->
          let
            newEntries = case model.entries of
              Success repositories ->
                Success (List.map (\r -> if r.id == repository then { r | authTokens = List.filter (\a -> a.id /= authTokenId) r.authTokens } else r) repositories)
              Failure -> model.entries
              Loading -> model.entries

          in
          ( { model | entries = newEntries }
          , closeDialogById (deleteTokenDialogId authTokenId)
          , ConfirmedUserIsLoggedIn
          )

    SendCreateTokenRequest repositoryName permissionLevel ->
      ( model
      , Http.send ReceivedCreateTokenConfirmation <|
        Http.post
          ("/dashboard/repository/" ++ (RepositoryId.toString repositoryName) ++ "/token?permission=" ++ PermissionLevel.toStringInUrlQueryParam permissionLevel)
          Http.emptyBody
          newlyCreatedAuthTokenDecoder
      , ConfirmedUserIsLoggedIn
      )

    ReceivedCreateTokenConfirmation result ->
      case Debug.log "createtokenconfirmation" result of
        Err httpError ->
            -- FIXME: Deal with this better; instead of failing the entire model, we should have some sort of failure modal
            ({ model | entries = Failure }, Cmd.none, httpErrorToLoginUpdate httpError)

        Ok (authToken, newlyCreatedTokenValue) ->
          let
            newEntries = case model.entries of
              Success repositories ->
                repositories
                  |> List.map (\r -> if r.id == authToken.repository then { r | authTokens = authToken :: r.authTokens, newTokenValue = Just newlyCreatedTokenValue } else r )
                  |> Success
              Failure -> model.entries
              Loading -> model.entries
          in
          ( { model | entries = newEntries }
          , Cmd.batch[closeDialogById (createTokenDialogId authToken.repository), openDialogById (tokenSuccessfullyCreatedDialogId authToken.repository)]
          , ConfirmedUserIsLoggedIn
          )

    CloseCreateTokenDialog repositoryName ->
      ( case model.entries of
        Success repositories -> { model | entries = Success (List.map (\r -> if r.id == repositoryName then { r | newTokenValue = Nothing } else r ) repositories) }
        Failure -> model
        Loading -> model
      , closeDialogById (createTokenDialogId repositoryName)
      , ConfirmedUserIsLoggedIn
      )

    SendCreateRepositoryRequest ->
      ( model
      , Http.send ReceivedCreateRepositoryConfirmation <| Http.post
        (Url.absolute ["dashboard", "repository"] [ Url.string "repository-name" model.createRepositoryCurrentName, Url.string "repository-url-safe-name" model.createRepositoryCurrentUrlSafeName ])
        Http.emptyBody
        (Decode.map (\i -> RepositoryId (String.fromInt i)) Decode.int)
      , NoUpdateAboutLoginStatus
      )

    ReceivedCreateRepositoryConfirmation result ->
      case result of
        Err httpError ->
            -- FIXME: Deal with this better; instead of failing the entire model, we should have some sort of failure modal
            ({ model | entries = Failure }, closeDialogById createRepositoryDialogId, httpErrorToLoginUpdate httpError)

        Ok repositoryId ->
          ( case model.entries of
            Failure -> model
            Loading -> model
            Success repositories ->
              { model | entries = Success ({id=repositoryId, packages=[], authTokens=[], newTokenValue=Nothing} :: repositories)}
          , closeDialogById createRepositoryDialogId
          , ConfirmedUserIsLoggedIn
          )

    CreateRepositoryRequestRepositoryName string ->
      ( { model | createRepositoryCurrentName = string }
      , Cmd.none
      , NoUpdateAboutLoginStatus
      )

    CreateRepositoryRequestRepositoryUrlSafeName string ->
      ( { model | createRepositoryCurrentUrlSafeName = string }
      , Cmd.none
      , NoUpdateAboutLoginStatus
      )

    SendDeleteRepositoryRequest repositoryId ->
      ( model
      , Http.send (\result -> ReceivedDeleteRepositoryConfirmation (repositoryId, result)) <| httpDelete
        (Url.absolute ["dashboard", "repository", RepositoryId.toString repositoryId] [])
        ()
      , NoUpdateAboutLoginStatus
      )

    ReceivedDeleteRepositoryConfirmation result ->
      case result of
        (repositoryId, Err httpError) ->
            -- FIXME: Deal with this better; instead of failing the entire model, we should have some sort of failure modal
            ({ model | entries = Failure }, closeDialogById (deleteRepositoryDialogId repositoryId), httpErrorToLoginUpdate httpError)

        (repositoryId, Ok ()) ->
          ( case model.entries of
            Failure -> model
            Loading -> model
            Success repositories ->
              { model | entries = Success (List.filter (\r -> r.id /= repositoryId) repositories)}
          , closeDialogById (deleteRepositoryDialogId repositoryId)
          , ConfirmedUserIsLoggedIn
          )

    SendDeletePackageRequest record ->
      ( model
      , Http.send (\httpresult -> ReceivedDeletePackageConfirmation (record, httpresult)) <| httpDelete
        (Url.absolute ["dashboard", "repository", RepositoryId.toString record.repository, "packages", record.author, record.project, record.version] [])
        ()
      , NoUpdateAboutLoginStatus
      )

    ReceivedDeletePackageConfirmation (record, result) ->
      case result of
        Err httpError ->
            -- FIXME: Deal with this better; instead of failing the entire model, we should have some sort of failure modal
            ({ model | entries = Failure }, closeDialogById (deletePackageDialogId record.repository record.author record.project record.version), httpErrorToLoginUpdate httpError)

        Ok () ->
          ( case model.entries of
            Failure -> model
            Loading -> model
            Success repositories ->
              { model | entries = Success (List.map (\r -> {r | packages = removePackageFromList record r.packages}) repositories)}
          , closeDialogById (deletePackageDialogId record.repository record.author record.project record.version)
          , ConfirmedUserIsLoggedIn
          )



doesPackageIdentifierMatchPackage : PackageIdentifier -> Package -> Bool
doesPackageIdentifierMatchPackage packageIdentifier package =
    packageIdentifier.repository == RepositoryId package.repositoryId
      && packageIdentifier.author == package.author
      && packageIdentifier.project == package.project
      && packageIdentifier.version == V.toString package.version



removePackageFromList : PackageIdentifier -> List Package -> List Package
removePackageFromList packageIdentifier packages =
    List.filter (\p -> not (doesPackageIdentifierMatchPackage packageIdentifier p)) packages


closeDialogById : String -> Cmd msg
closeDialogById dialogId =
  toggleDialogById
    ( Encode.object
      [ ("dialogId", Encode.string dialogId)
      , ("action", Encode.string "close")
      ]
    )

openDialogById : String -> Cmd msg
openDialogById dialogId =
  toggleDialogById
    ( Encode.object
      [ ("dialogId", Encode.string dialogId)
      , ("action", Encode.string "open")
      ]
    )

httpDelete url valueToReturnOnSuccess = Http.request
  { method = "DELETE"
  , headers = []
  , url = url
  , body = Http.emptyBody
  -- We don't actually care about the response value at all
  , expect = Http.expectStringResponse (\_ -> Ok valueToReturnOnSuccess)
  , timeout = Nothing
  , withCredentials = False
  }

port toggleDialogById : Value -> Cmd msg

createTokenDialogId repositoryId = "create-token-dialog-" ++ (RepositoryId.toString repositoryId)

deleteTokenDialogId tokenId = ("delete-token-dialog-" ++ (AuthTokenId.toString tokenId))

createRepositoryDialogId = "create-repository-dialog"

deleteRepositoryDialogId repositoryId = "delete-repository-dialog-" ++ (RepositoryId.toString repositoryId)

deletePackageDialogId repositoryId author project version =
  "delete-package-dialog-" ++ (RepositoryId.toString repositoryId) ++ "-" ++ author ++ "-" ++ project ++ "-" ++ version

tokenSuccessfullyCreatedDialogId repositoryId = "token-creation-success-dialog-" ++ (RepositoryId.toString repositoryId)

searchByType : String -> Cmd msg
searchByType query =
  Nav.load <|
    Url.crossOrigin "https://klaftertief.github.io" [ "elm-search" ] [ Url.string "q" query ]


addQueryToUrl : Nav.Key -> Model -> Cmd msg
addQueryToUrl key model =
  Nav.replaceUrl key <|
    Url.absolute
      (case model.author of
         Nothing     -> []
         Just author -> [ "packages", author ]
      )
      (if String.isEmpty model.query then
        []
      else
        [ Url.string "q" model.query ]
      )



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
  { title = Maybe.withDefault "Elm Packages" model.author
  , header =
      case model.author of
        Nothing     -> []
        Just author ->
          case model.repository of
            Just repository -> [ Skeleton.authorSegment repository author ]
            Nothing -> []
  , warning = Skeleton.NoProblems
  , attrs = []
  , kids =
      [ lazy5 viewSearch model.query model.author model.entries model.createRepositoryCurrentName model.createRepositoryCurrentUrlSafeName
      , viewSidebar
      ]
  }



-- VIEW SEARCH


viewSearch : String -> Maybe String -> Entries -> String -> String -> Html Msg
viewSearch query author entries createRepoFormRepoName createRepoFormRepoUrlSafeName =
  div [ class "catalog" ]
    [ Html.form
        [ onSubmit QuerySubmitted
        ]
        [ input
            [ placeholder "Search"
            , value query
            , onInput QueryChanged
            , autofocus True
            , name "q"
            , autocomplete False
            ]
            []
        ]
    , button
      [ onClick (OpenGenericDialogById createRepositoryDialogId) ]
      [ text "Create new repository" ]
    , node "dialog"
      [ Html.Attributes.id createRepositoryDialogId ]
      [ form
        [ onSubmit SendCreateRepositoryRequest ]
        [ label
          [ Html.Attributes.for "repository-name" ]
          [ text "Repository human readable name:" ]
        , input
          [ Html.Attributes.id "repository-name", value createRepoFormRepoName, onInput CreateRepositoryRequestRepositoryName ]
          []
        , label
          [ Html.Attributes.for "repository-url-safe-name" ]
          [ text "Repository URL safe name:" ]
        , input
          [ Html.Attributes.id "repository-url-safe-name", value createRepoFormRepoUrlSafeName, onInput CreateRepositoryRequestRepositoryUrlSafeName ]
          []
        , button [ type_ "submit" ] [ text "Create Repository" ]
        ]
      , button [ onClick (CloseGenericDialogById createRepositoryDialogId) ] [ text "Cancel" ]
      ]
    , case entries of
        Failure ->
          div Problem.styles (Problem.offline "search.json")

        Loading ->
          text "" -- TODO

        Success es ->
          let
            results =
              Entry.search query author es
          in
          viewAllRepositories query results
    ]


-- VIEW BACKEND DATA


viewAuthToken : Entry.AuthToken -> Html Msg
viewAuthToken authToken =
  li
    [ style "list-style-type" "none" ]
    [ div
      []
      [ h5 [] [ text "Token Value (only initial fragment is shown for security reasons)" ]
      , text (authToken.fragment ++ "...")
      ]
    , div
      []
      [ h5 [] [ text "Permission Level" ]
      , case authToken.permission of
          PermissionLevel.ReadWrite -> text "Read/Write"
          PermissionLevel.ReadOnly -> text "Read Only"
      ]
    , button
      [ onClick (OpenGenericDialogById (deleteTokenDialogId authToken.id)) ]
      [ text "Delete this token" ]
    , node "dialog"
      [ Html.Attributes.id (deleteTokenDialogId authToken.id) ]
      [ div [] [ text ("Are you sure you want to delete token " ++ authToken.fragment ++ "...?") ]
      , button
        -- TODO: Add confirmation spinner
        [ onClick (SendDeleteTokenRequest authToken.repository authToken.id) ]
        [ text "Delete this token" ]
      , button
        [ onClick (CloseGenericDialogById (deleteTokenDialogId authToken.id)) ]
        [ text "Cancel" ]
      ]
    ]


viewAllRepositories : String -> List Entry.Repository -> Html Msg
viewAllRepositories query repositories = repositories
    |> List.map (\repository -> div [] [ h2 [] [ text ("Repository:" ++ (RepositoryId.toString repository.id)) ], viewRepository query repository ])
    |> div []


viewRepository : String -> Entry.Repository -> Html Msg
viewRepository query repository =
  let
    results =
      repository.packages
        |> groupBy (\p -> (p.author, p.project))
        |> Dict.map (\_ differentVersionsOfPackage -> NonEmptyList.maxIn (\p -> V.toString p.version) differentVersionsOfPackage)
        |> Dict.values
        |> List.map viewPackage
  in
  div
    []
    [ details []
      [ summary [] [ span [ style "font-weight" "bold" ] [ text "API Auth Tokens" ]]
      , ul [] (List.map viewAuthToken repository.authTokens)
      , button
        [ onClick (OpenGenericDialogById (createTokenDialogId repository.id)) ]
        [ text "Create new auth token" ]
    , node "dialog"
      [ Html.Attributes.id (createTokenDialogId repository.id) ]
      [ button
        -- TODO: Add confirmation spinner
        [ onClick (SendCreateTokenRequest repository.id PermissionLevel.ReadWrite) ]
        [ text "Create token with both package read and package publish permissions" ]
      , button
        -- TODO: Add confirmation spinner
        [ onClick (SendCreateTokenRequest repository.id PermissionLevel.ReadOnly) ]
        [ text "Create token with only package read permissions" ]
      , button
        [ onClick (CloseGenericDialogById (createTokenDialogId repository.id)) ]
        [ text "Cancel" ]
      ]
      ]
    , node "dialog"
      [ Html.Attributes.id (tokenSuccessfullyCreatedDialogId repository.id) ]
      [ div
        []
        [ text "Your new authentication token has the following value. Store it somewhere safe! For security purposes, as soon as you close this window this value will never be shown again!"
        , pre [] [ text (Maybe.withDefault "ERROR UNKNOWN TOKEN VALUE" repository.newTokenValue) ]
        ]
      , button
        [ onClick (CloseGenericDialogById (tokenSuccessfullyCreatedDialogId repository.id)) ]
        [ text "Close" ]
      ]
    , div []
      [ Keyed.node "div" [] <|
          -- FIXME: Originally was the following:
          --("h", viewHint (List.isEmpty results) query) :: results
          -- Figure out what the hint was for
          if (List.isEmpty results)
            then [("h", div [] [ text "Use ", pre [] [ text "zokka publish" ], text " to publish a new package to this repository!"])]
            else results
      ]
    , button
      [ onClick (OpenGenericDialogById (deleteRepositoryDialogId repository.id)) ]
      [ text "Delete this repository" ]
    , node "dialog"
      [ Html.Attributes.id (deleteRepositoryDialogId repository.id) ]
      [ div
        []
        [ text ("Are you sure you want to delete repository " ++ (RepositoryId.toString repository.id) ++ "? ")
        , text "You will delete every package and authentication token in this repository as well!"
        ]
      , button
        [ onClick (SendDeleteRepositoryRequest repository.id)]
        [ text ("Delete repository " ++ RepositoryId.toString repository.id)]
      , button
        [ onClick (CloseGenericDialogById (deleteRepositoryDialogId repository.id)) ]
        [ text "Cancel" ]
      ]
    ]



-- VIEW ENTRY


viewPackage : Package -> (String, Html Msg)
viewPackage entry =
  ( entry.author ++ "/" ++ entry.project
  , lazy viewPackageHelp entry
  )


viewPackageHelp : Package -> Html Msg
viewPackageHelp ({ repositoryId, author, project, summary } as entry) =
  div [ class "pkg-summary" ]
    [ div [ class "pkg-summary-title" ]
        [ h1 []
            [ a [ href (Href.toVersion repositoryId author project Nothing Nothing) ]
                [ span [ class "pkg-summary-author" ] [ text (author ++ "/") ]
                , wbr [] []
                , span [ class "pkg-summary-project" ] [ text project ]
                ]
            ]
        , viewLatestVersion entry
        ]
    , p [ class "pkg-summary-desc" ] [ text summary ]
    ]


viewLatestVersion : Package -> Html msg
viewLatestVersion entry =
  div [ class "pkg-summary-version" ] <|
    case V.toTuple entry.version of
      (1, 0, 0) ->
        [ a
            [ href (Href.toVersion entry.repositoryId entry.author entry.project (Just entry.version) Nothing) ]
            [ text (V.toString entry.version) ]
        ]

      _ ->
        [ a
            [ href (Href.toProject entry.repositoryId entry.author entry.project) ]
            [ text "… " ]
        , a
            [ href (Href.toVersion entry.repositoryId entry.author entry.project (Just entry.version) Nothing) ]
            [ text (V.toString entry.version) ]
        ]




-- VIEW SIDEBAR


viewSidebar : Html msg
viewSidebar =
  div [ class "catalog-sidebar" ]
    [ h2 [] [ text "Resources" ]
    , ul []
        [ li [] [ a [ href "https://klaftertief.github.io/elm-search/" ] [ text "Search by Type" ] ]
        , li [] [ a [ href "https://guide.elm-lang.org/install/elm.html#elm-install" ] [ text "Using Packages" ] ]
        , li [] [ a [ href "/help/design-guidelines" ] [ text "API Design Guidelines" ] ]
        , li [] [ a [ href "/help/documentation-format" ] [ text "Write great docs" ] ]
        , li [] [ a [ href "https://elm-lang.org" ] [ text "Elm Website" ] ]
        ]
    ]



-- VIEW HINTS


viewHint : Bool -> String -> Html msg
viewHint noAlts query =
  viewHintHelp noAlts (String.toLower (String.replace "-" " " query)) hints


viewHintHelp : Bool -> String -> List (Hint msg) -> Html msg
viewHintHelp noAlts query remainingHints =
  case remainingHints of
    [] ->
      text ""

    hint :: otherHints ->
      if String.startsWith query hint.term && (noAlts || String.length query >= hint.min) then
        hint.html
      else
        viewHintHelp noAlts query otherHints


type alias Hint msg =
  { term : String
  , min : Int
  , html : Html msg
  }


hints : List (Hint msg)
hints =
  [ Hint "spa" 3 singlePageApp
  , Hint "single page app" 5 singlePageApp
  , Hint "components" 5 components
  , Hint "router" 4 router
  , Hint "routing" 4 router
  , Hint "routes" 4 router
  , Hint "focus" 4 focus
  , Hint "blur" 4 focus
  , Hint "scroll" 4 scroll
  , Hint "scrollheight" 7 scroll
  , Hint "scrollwidth" 7 scroll
  , Hint "scrollx" 7 scroll
  , Hint "scrolly" 7 scroll
  , Hint "scrollto" 7 scroll
  , Hint "scrollintoview" 7 scroll
  , Hint "mouse" 4 mouse
  , Hint "keyboard" 4 keyboard
  , Hint "window" 4 window
  , Hint "visibility" 5 window
  , Hint "animation" 5 animation
  , Hint "requestanimationframe" 8 animation
  , Hint "lenses" 4 lenses
  ]


makeHint : List (Html msg) -> Html msg
makeHint message =
  p [ class "pkg-hint" ] <|
    b [] [ text "Hint:" ] :: text " " :: message


singlePageApp : Html msg
singlePageApp =
  makeHint
    [ text "All single-page apps in Elm use "
    , codeLink "https://package.elm-lang.org/packages/elm/browser/latest/" "elm/browser"
    , text " to control the URL, with help from "
    , codeLink "https://package.elm-lang.org/packages/elm/url/latest/" "elm/url"
    , text " convert between URLs and nice structured data. I very highly recommend working through "
    , guide
    , text " to learn how! Once you have made one or two single-page apps the standard way, it will be much easier to tell which (if any) of the packages below can make your code any easier."
    ]


components : Html msg
components =
  makeHint
    [ text "Components are objects!"
    , ul [ style "list-style-type" "none" ]
        [ li [] [ text "Components = Local State + Methods" ]
        , li [] [ text "Local State + Methods = Objects" ]
        ]
    , text "We get very few folks asking how to structure Elm code with objects. Elm does not have objects! We get a lot of folks asking about how to use components, but it is essentially the same question. Elm emphasizes "
    , i [] [ text "functions" ]
    , text " instead. Folks usually have the best experience if they follow the advice in "
    , guide
    , text " and "
    , a [ href "https://youtu.be/XpDsk374LDE" ] [ text "The Life of a File" ]
    , text ", exploring and understanding the techniques specific to Elm "
    , i [] [ text "before" ]
    , text " trying to bring in techniques from other languages."
    ]


router : Html msg
router =
  makeHint
    [ text "The "
    , codeLink "https://package.elm-lang.org/packages/elm/url/latest/" "elm/url"
    , text " package has everything you need to turn paths, queries, and hashes into useful data. But definitely work through "
    , guide
    , text " to learn how this fits into a "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser" (Just "application")) "Browser.application"
    , text " that manages the URL!"
    ]


focus : Html msg
focus =
  makeHint
    [ text "Check out "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Dom" Nothing) "Browser.Dom"
    , text " for focusing on certain nodes. It uses tasks, so be sure you have learned about "
    , code [] [ text "Cmd" ]
    , text " values in "
    , guide
    , text " and then read through the "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "core" Nothing "Task" Nothing) "Task"
    , text " module so you do not have to guess at how anything works!"
    ]


scroll : Html msg
scroll =
  makeHint
    [ text "Check out "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Dom" Nothing) "Browser.Dom"
    , text " for getting and setting scroll positions. It uses tasks, so be sure you have learned about "
    , code [] [ text "Cmd" ]
    , text " values in "
    , guide
    , text " and then read through the "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "core" Nothing "Task" Nothing) "Task"
    , text " module so you do not have to guess at how anything works!"
    ]


mouse : Html msg
mouse =
  makeHint
    [ text "Folks usually use "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "html" Nothing "Html.Events" Nothing) "Html.Events"
    , text " to detect clicks on buttons. If you want mouse events for the whole page, you may want "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Events" Nothing) "Browser.Events"
    , text " instead. Reading "
    , guide
    , text " should give the foundation for using either!"
    ]


keyboard : Html msg
keyboard =
  makeHint
    [ text "Folks usually use "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "html" Nothing "Html.Events" Nothing) "Html.Events"
    , text " for key presses in text fields. If you want keyboard events for the whole page, you may want "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Events" Nothing) "Browser.Events"
    , text " instead. Reading "
    , guide
    , text " should give the foundation for using either!"
    ]


window : Html msg
window =
  makeHint
    [ text "Use "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Dom" Nothing) "Browser.Dom"
    , text " to get the current window size, and use "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Events" Nothing) "Browser.Events"
    , text " to detect when the window changes size or is not visible at the moment."
    ]


animation : Html msg
animation =
  makeHint
    [ text "If you are not using CSS animations, you will need "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Events" (Just "onAnimationFrame")) "onAnimationFrame"
    , text " to get smooth animations. The packages below may make one of these paths easier for you, but sometimes it is easier to just do things directly!"
    ]


lenses : Html msg
lenses =
  makeHint
    [ text "Lenses are not commonly used in Elm. Their design focuses on manipulating deeply nested data structures, like records in records in dictionaries in lists. But rather than introducing a complex system to help with already complex data structures, we encourage folks to first work on simplifying the data structure."
    , br [] []
    , br [] []
    , text "Maybe this means flattening records. Or using "
    , a [ href "https://guide.elm-lang.org/types/custom_types.html" ] [ text "custom types" ]
    , text " to model different possibilities more precisely. Or representing graphs with "
    , codeText "Dict"
    , text " values as described "
    , a [ href "https://evancz.gitbooks.io/functional-programming-in-elm/graphs/" ] [ text "here" ]
    , text ". Or using the module system to create strong boundaries, using opaque types with helper functions to contain complexity."
    , br [] []
    , br [] []
    , text "Point is, there are many paths to explore that will produce easier code with stronger guarantees, and folks are always happy to help if you share your situation on "
    , a [ href "http://elmlang.herokuapp.com/" ] [ text "Slack" ]
    , text " or "
    , a [ href "https://discourse.elm-lang.org/" ] [ text "Discourse" ]
    , text "!"
    ]


guide : Html msg
guide =
  codeLink "https://guide.elm-lang.org" "guide.elm-lang.org"


codeLink : String -> String -> Html msg
codeLink url txt =
  a [ href url ] [ codeText txt ]


codeText : String -> Html msg
codeText txt =
  code [] [ text txt ]
