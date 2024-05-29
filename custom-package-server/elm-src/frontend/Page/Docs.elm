module Page.Docs exposing
  ( Model
  , Focus(..)
  , init
  , Msg
  , update
  , view
  , toTitle
  )


import Browser.Dom as Dom
import Elm.Constraint as Constraint exposing (Constraint)
import Elm.Docs as Docs
import Elm.License as License
import Elm.Package as Package
import Elm.Project as Project
import Elm.Version as V
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Http
import Href
import Page.Docs.Block as Block
import Page.Problem as Problem
import Release
import Session
import Skeleton
import Task
import Url.Builder as Url
import Utils.LoginUpdate exposing (LoginUpdate(..), httpErrorToLoginUpdate)
import Utils.Markdown as Markdown
import Utils.OneOrMore exposing (OneOrMore)
import Time



-- MODEL


type alias Model =
  { session : Session.Data
  , repository : String
  , author : String
  , project : String
  , version : Maybe V.Version
  , focus : Focus
  , query : String
  , latest : Status V.Version
  , readme : Status String
  , docs : Status (List Docs.Module)
  , manifest : Status Project.PackageInfo
  , time : Status Time.Posix
  }


type alias Info =
  { readme : String
  , docs : List Docs.Module
  , manifest : Project.PackageInfo
  }


type Focus
  = Readme (Maybe String)
  | About
  | Module String (Maybe String)


type Status a
  = Failure
  | Loading
  | Success a


type DocsError
  = NotFound
  | FoundButMissingModule



-- INIT


init : Session.Data -> String -> String -> String -> Maybe V.Version -> Focus -> Maybe String -> ( Model, Cmd Msg, LoginUpdate )
init session repository author project version focus query =
  let
    model =
      { session = session
      , repository = repository
      , author = author
      , project = project
      , version = version
      , focus = focus
      , query = Maybe.withDefault "" query
      , latest = Loading
      , readme = Loading
      , docs = Loading
      , manifest = Loading
      , time = Loading
      }
  in
  case Session.getReleases session author project of
    Just releases ->
      let
        latest = Release.getLatestVersion releases
        time =
          Release.getTime (Maybe.withDefault latest version) releases
            |> Maybe.map Success
            |> Maybe.withDefault Failure
        (newModel, cmd) = getInfo latest { model | latest = Success latest, time = time }
      in
      (newModel, cmd, NoUpdateAboutLoginStatus)

    Nothing ->
      ( model
      , Http.send GotReleases (Session.fetchReleases repository author project)
      , NoUpdateAboutLoginStatus
      )


getInfo : V.Version -> Model -> ( Model, Cmd Msg )
getInfo latest model =
  let
    author = model.author
    project = model.project
    repository = model.repository
    version = Maybe.withDefault latest model.version
    maybeInfo =
      Maybe.map3 Info
        (Session.getReadme model.session author project version)
        (Session.getDocs model.session author project version)
        (Session.getManifest model.session author project version)
  in
  case maybeInfo of
    Nothing ->
      ( model
      , Cmd.batch
          [ Http.send (GotReadme version) (Session.fetchReadme repository author project version)
          , Http.send (GotDocs version) (Session.fetchDocs repository author project version)
          , Http.send (GotManifest version) (Session.fetchManifest repository author project version)
          ]
      )

    Just info ->
      ( { model
            | readme = Success info.readme
            , docs = Success info.docs
            , manifest = Success info.manifest
        }
      , scrollIfNeeded model.focus
      )


scrollIfNeeded : Focus -> Cmd Msg
scrollIfNeeded focus =
  case focus of
    Module _ (Just tag) -> scrollTo tag
    Module _ Nothing -> scrollToTop
    Readme (Just tag) -> scrollTo tag
    Readme Nothing -> scrollToTop
    About -> Cmd.none


scrollToTop : Cmd Msg
scrollToTop =
  Task.attempt ScrollAttempted (Dom.setViewport 0 0)


scrollTo : String -> Cmd Msg
scrollTo elementId =
  Task.attempt ScrollAttempted (
    Dom.getElement elementId
      |> Task.andThen (\info -> Dom.setViewport 0 info.element.y)
  )


scrollViewportTo : String -> String -> Cmd Msg
scrollViewportTo viewportId elementId =
  Task.attempt ScrollAttempted (
    Task.map3 (\_ a b -> (a, b))
        (Dom.setViewportOf viewportId 0 0)
        (Dom.getElement viewportId)
        (Dom.getElement elementId)
        |> Task.andThen
            (\(viewport, target) ->
              Dom.setViewportOf viewportId 0 (target.element.y - viewport.element.y)
            )
  )


-- UPDATE


type Msg
  = QueryChanged String
  | ScrollAttempted (Result Dom.Error ())
  | GotReleases (Result Http.Error (OneOrMore Release.Release))
  | GotReadme V.Version (Result Http.Error String)
  | GotDocs V.Version (Result Http.Error (List Docs.Module))
  | GotManifest V.Version (Result Http.Error Project.PackageInfo)


update : Msg -> Model -> ( Model, Cmd Msg, LoginUpdate )
update msg model =
  case msg of
    QueryChanged query ->
      ( { model | query = query }
      , case model.focus of
          Module name _ -> scrollViewportTo "pkg-nav-modules" ("nav-" ++ name)
          _ -> Cmd.none
      , NoUpdateAboutLoginStatus
      )

    ScrollAttempted _ ->
      ( model
      , Cmd.none
      , NoUpdateAboutLoginStatus
      )

    GotReleases result ->
      case result of
        Err httpError ->
          ( { model
                | latest = Failure
                , readme = Failure
                , docs = Failure
                , manifest = Failure
            }
          , Cmd.none
          , httpErrorToLoginUpdate httpError
          )

        Ok releases ->
          let
            latest = Release.getLatestVersion releases
            time =
              Release.getTime (Maybe.withDefault latest model.version) releases
                |> Maybe.map Success
                |> Maybe.withDefault Failure
            (newModel, cmd) =
              getInfo latest
                { model
                    | latest = Success latest
                    , time = time
                    , session = Session.addReleases model.author model.project releases model.session
                }
          in
          ( newModel, cmd, NoUpdateAboutLoginStatus )

    GotReadme version result ->
      case result of
        Err httpError ->
          ( { model | readme = Failure }
          , Cmd.none
          , httpErrorToLoginUpdate httpError
          )

        Ok readme ->
          ( { model
                | readme = Success readme
                , session = Session.addReadme model.author model.project version readme model.session
            }
          , scrollIfNeeded model.focus
          , ConfirmedUserIsLoggedIn
          )

    GotDocs version result ->
      case result of
        Err httpError ->
          ( { model | docs = Failure }
          , Cmd.none
          , httpErrorToLoginUpdate httpError
          )

        Ok docs ->
          ( { model
                | docs = Success docs
                , session = Session.addDocs model.author model.project version docs model.session
            }
          , scrollIfNeeded model.focus
          , ConfirmedUserIsLoggedIn
          )

    GotManifest version result ->
      case result of
        Err httpError ->
          ( { model | manifest = Failure }
          , Cmd.none
          , httpErrorToLoginUpdate httpError
          )

        Ok manifest ->
          ( { model
                | manifest = Success manifest
                , session = Session.addManifest model.author model.project version manifest model.session
            }
          , Cmd.none
          , ConfirmedUserIsLoggedIn
          )


-- VIEW


view : Model -> Int -> Skeleton.Details Msg
view model width =
  { title = toTitle model
  , header = toHeader model
  , warning = toWarning model
  , attrs = []
  , kids =
      [ viewContent model width
      , viewSidebar model
      ]
  }



-- TITLE


toTitle : Model -> String
toTitle model =
  case model.focus of
    Readme _ ->
      toGenericTitle model

    About ->
      toGenericTitle model

    Module name _ ->
      name ++ " - " ++ toGenericTitle model


toGenericTitle : Model -> String
toGenericTitle model =
  case getVersion model of
    Just version ->
      model.project ++ " " ++ V.toString version

    Nothing ->
      model.project


getVersion : Model -> Maybe V.Version
getVersion model =
  case model.version of
    Just version ->
      model.version

    Nothing ->
      case model.latest of
        Success version -> Just version
        Loading -> Nothing
        Failure -> Nothing



-- TO HEADER


toHeader : Model -> List Skeleton.Segment
toHeader model =
  [ Skeleton.authorSegment model.repository model.author
  , Skeleton.projectSegment model.repository model.author model.project
  , Skeleton.versionSegment model.repository model.author model.project (getVersion model)
  ]



-- WARNING


toWarning : Model -> Skeleton.Warning
toWarning model =
  case model.version of
    Nothing ->
      toElmWarning model.manifest

    Just version ->
      case model.latest of
        Success latest ->
          if version == latest then
            toElmWarning model.manifest
          else
            Skeleton.NewerVersion (toNewerUrl model) latest

        Loading ->
          toElmWarning model.manifest

        Failure ->
          toElmWarning model.manifest


toElmWarning : Status Project.PackageInfo -> Skeleton.Warning
toElmWarning status =
  case (status, V.fromString "0.19.1") of
    (Success manifest, Just elm_0_19_1) ->
      if Constraint.check elm_0_19_1 manifest.elm then
        Skeleton.NoProblems
      else
        Skeleton.IncompatibleElm elm_0_19_1

    _ ->
      Skeleton.NoProblems


toNewerUrl : Model -> String
toNewerUrl model =
  case model.focus of
    Readme tag ->
      Href.toVersion model.repository model.author model.project Nothing tag

    About ->
      Href.toAbout model.repository model.author model.project Nothing

    Module name tag ->
      Href.toModule model.repository model.author model.project Nothing name tag



-- VIEW CONTENT


viewContent : Model -> Int -> Html msg
viewContent model width =
  case model.focus of
    Readme _ ->
      lazy viewReadme model.readme

    About ->
      lazy2 viewAbout model.manifest model.time

    Module name tag ->
      lazy7 viewModule model.repository model.author model.project model.version name model.docs width



-- VIEW README


viewReadme : Status String -> Html msg
viewReadme status =
  case status of
    Success readme ->
      div [ class "block-list" ] [ Markdown.block readme ]

    Loading ->
      div [ class "block-list" ] [ text "" ] -- TODO

    Failure ->
      div
        (class "block-list" :: Problem.styles)
        (Problem.offline "README.md")



-- VIEW MODULE


viewModule : String -> String -> String -> Maybe V.Version -> String -> Status (List Docs.Module) -> Int -> Html msg
viewModule repository author project version name status width =
  let
    breakableName =
      name
        |> String.split "."
        |> List.map text
        |> List.intersperse (span [] [ text ".", wbr [] [] ])
  in
  case status of
    Success allDocs ->
      case findModule name allDocs of
        Just docs ->
          let
            header = h1 [class "block-list-title"] breakableName
            info = Block.makeInfo repository author project version name allDocs width
            blocks = List.map (Block.view info) (Docs.toBlocks docs)
          in
          div [ class "block-list" ] (header :: blocks)

        Nothing ->
          div
            (class "block-list" :: Problem.styles)
            (Problem.missingModule repository author project version name)

    Loading ->
      div [ class "block-list" ]
        [ h1 [class "block-list-title"] breakableName -- TODO better loading
        ]

    Failure ->
      div
        (class "block-list" :: Problem.styles)
        (Problem.offline "docs.json")


findModule : String -> List Docs.Module -> Maybe Docs.Module
findModule name docsList =
  case docsList of
    [] ->
      Nothing

    docs :: otherDocs ->
      if docs.name == name then
        Just docs
      else
        findModule name otherDocs



-- VIEW SIDEBAR


viewSidebar : Model -> Html Msg
viewSidebar model =
  div
    [ class "pkg-nav"
    ]
    [ ul [ class "pkg-nav-links" ]
        [ li [] [ lazy5 viewReadmeLink model.repository model.author model.project model.version model.focus ]
        , li [] [ lazy5 viewAboutLink model.repository model.author model.project model.version model.focus ]
        , li [] [ lazy4 viewBrowseSourceLink model.author model.project model.version model.latest ]
        ]
    , h2 [] [ text "Modules" ]
    , input
        [ placeholder "Search"
        , value model.query
        , onInput QueryChanged
        , size 1
        , autocomplete False
        ]
        []
    , viewSidebarModules model
    ]


viewSidebarModules : Model -> Html msg
viewSidebarModules model =
  case model.docs of
    Failure ->
      text "" -- TODO

    Loading ->
      text "" -- TODO

    Success modules ->
      if String.isEmpty model.query then
        let
          viewEntry docs =
            li [] [ viewModuleLink model docs.name ]
        in
        ul [ id "pkg-nav-modules" ]
          (List.map viewEntry modules)

      else
        let
          query =
            -- String.trim allows to display all items with a space
            String.toLower (String.trim model.query)
        in
        ul [ id "pkg-nav-modules" ]
          (List.filterMap (viewSearchItem model query) modules)


viewSearchItem : Model -> String -> Docs.Module -> Maybe (Html msg)
viewSearchItem model query docs =
  let
    toItem ownerName valueName =
      viewValueItem model docs.name ownerName valueName

    matches =
      List.filterMap (isMatch query toItem) docs.binops
      ++ List.concatMap (isUnionMatch query toItem) docs.unions
      ++ List.filterMap (isMatch query toItem) docs.aliases
      ++ List.filterMap (isMatch query toItem) docs.values
  in
    if List.isEmpty matches && not (String.contains query docs.name) then
      Nothing

    else
      Just <|
        li
          [ class "pkg-nav-search-chunk"
          ]
          [ viewModuleLink model docs.name
          , ul [] matches
          ]


isMatch : String -> (String -> String -> b) -> { r | name : String } -> Maybe b
isMatch query toResult {name} =
  if String.contains query (String.toLower name) then
    Just (toResult name name)
  else
    Nothing


isUnionMatch : String -> (String -> String -> a) -> Docs.Union -> List a
isUnionMatch query toResult {name,tags} =
  let
    tagMatches =
      List.filterMap (isTagMatch query toResult name) tags
  in
    if String.contains query (String.toLower name) then
      toResult name name :: tagMatches
    else
      tagMatches


isTagMatch : String -> (String -> String -> a) -> String -> (String, details) -> Maybe a
isTagMatch query toResult tipeName (tagName, _) =
  if String.contains query (String.toLower tagName) then
    Just (toResult tipeName tagName)
  else
    Nothing



-- VIEW "README" LINK


viewReadmeLink : String -> String -> String -> Maybe V.Version -> Focus -> Html msg
viewReadmeLink repository author project version focus =
  navLink "README" (Href.toVersion repository author project version Nothing) <|
    case focus of
      Readme _ -> True
      About -> False
      Module _ _ -> False



-- VIEW "ABOUT" LINK


viewAboutLink : String -> String -> String -> Maybe V.Version -> Focus -> Html msg
viewAboutLink repository author project version focus =
  navLink "About" (Href.toAbout repository author project version) <|
    case focus of
      Readme _ -> False
      About -> True
      Module _ _ -> False



-- VIEW "BROWSE SOURCE" LINK


viewBrowseSourceLink : String -> String -> Maybe V.Version -> Status V.Version -> Html msg
viewBrowseSourceLink author project maybeVersion latest =
  case maybeVersion of
    Just version ->
      viewBrowseSourceLinkHelp author project version

    Nothing ->
      case latest of
        Success version ->
          viewBrowseSourceLinkHelp author project version

        Loading ->
          text "Source"

        Failure ->
          text "Source"


viewBrowseSourceLinkHelp : String -> String -> V.Version -> Html msg
viewBrowseSourceLinkHelp author project version =
  let
    url =
      --FIXME: This needs to move away from GitHub links
      Url.crossOrigin
        "https://github.com"
        [ author, project, "tree", V.toString version ]
        []
  in
  a [ href url ] [ text "Source" ]



-- VIEW "MODULE" LINK


viewModuleLink : Model -> String -> Html msg
viewModuleLink model name =
  let
    url =
      Href.toModuleWithQuery model.repository model.author model.project model.version name Nothing model.query
  in
  navModuleLink name url <|
    case model.focus of
      Readme _ ->
        False

      About ->
        False

      Module selectedName _ ->
        selectedName == name


viewValueItem : Model -> String -> String -> String -> Html msg
viewValueItem { repository, author, project, version, query } moduleName ownerName valueName =
  let
    url =
      Href.toModuleWithQuery repository author project version moduleName (Just ownerName) query
  in
  li [ class "pkg-nav-value" ] [ navModuleLink valueName url False ]



-- VIEW ABOUT


viewAbout : Status Project.PackageInfo -> Status Time.Posix -> Html msg
viewAbout manifestStatus timeStatus =
  case manifestStatus of
    Success manifest ->
      div [ class "block-list pkg-about" ]
        [ h1 [] [ text "About" ]
        , text manifest.summary
        , viewInstall manifest
        , viewRelease manifest timeStatus
        , viewLicense manifest
        , viewDependencies manifest
        ]

    Loading ->
      div [ class "block-list pkg-about" ] [ text "" ] -- TODO

    Failure ->
      div
        (class "block-list pkg-about" :: Problem.styles)
        (Problem.offline "elm.json")


viewInstall : Project.PackageInfo -> Html msg
viewInstall manifest =
  case V.fromString "0.19.0" of
    Just elm019 ->
      if Constraint.check elm019 manifest.elm then
          viewInstallHelp "elm install" (Package.toString manifest.name)

      else
          viewInstallHelp "elm-package install" (Package.toString manifest.name)

    Nothing ->
      text ""


viewInstallHelp : String -> String -> Html msg
viewInstallHelp command package =
  div []
    [ h2 [] [ text "Install" ]
    , pre []
      [ code [] [ text (command ++ " " ++ package) ]
      ]
    ]


viewRelease : Project.PackageInfo -> Status Time.Posix -> Html msg
viewRelease manifest timeStatus =
  div []
    [ h2 [] [ text "Release" ]
    , viewTime manifest timeStatus
    ]


viewLicense : Project.PackageInfo -> Html msg
viewLicense manifest =
  let
    licenseUrl =
      Url.crossOrigin
        "https://github.com"
        [ Package.toString manifest.name, "blob", V.toString manifest.version, "LICENSE" ]
        []
  in
  div []
    [ h2 [] [ text "License" ]
    , a [ href licenseUrl ] [ text (License.toString manifest.license) ]
    ]


viewTime : Project.PackageInfo -> Status Time.Posix -> Html msg
viewTime manifest timeStatus =
  case timeStatus of
    Failure ->
      text ""

    Loading ->
      text ""

    Success time ->
      let
        releasesUrl =
          Url.crossOrigin "https://github.com"
            [ Package.toString manifest.name
            , "releases"
            , "tag"
            , V.toString manifest.version
            ]
            []
      in
      a [ href releasesUrl ]
        [ text <|
            String.concat
              [ monthToString (Time.toMonth Time.utc time)
              , " "
              , String.fromInt (Time.toDay Time.utc time)
              , ", "
              , String.fromInt (Time.toYear Time.utc time)
              ]
        ]


monthToString : Time.Month -> String
monthToString month =
  case month of
    Time.Jan -> "January"
    Time.Feb -> "February"
    Time.Mar -> "March"
    Time.Apr -> "April"
    Time.May -> "May"
    Time.Jun -> "June"
    Time.Jul -> "July"
    Time.Aug -> "August"
    Time.Sep -> "September"
    Time.Oct -> "October"
    Time.Nov -> "November"
    Time.Dec -> "December"



-- VIEW DEPENDENCIES


viewDependencies : Project.PackageInfo -> Html msg
viewDependencies manifest =
  div [ class "pkg-deps" ]
    [ h2 [] [ text "Dependencies" ]
    , ul [] <|
        li [] [ viewElmDependency manifest.elm ]
          :: (manifest.deps |> List.map viewDependency)
    ]


viewElmDependency : Constraint -> Html msg
viewElmDependency constraint =
  li []
    [ a [ href "https://guide.elm-lang.org/install/elm.html" ]
        [ text "elm" ]
    , text " "
    , text (Constraint.toString constraint)
    ]


viewDependency : (Package.Name, Constraint) -> Html msg
viewDependency (packageName, constraint) =
  let
    name = Package.toString packageName
  in
  li []
    [ a [ href (Url.absolute [ "packages", name, "latest", "" ] []) ]
        [ text name ]
    , text " "
    , span [ class "pkg-constraint" ] [ text (Constraint.toString constraint) ]
    ]


constraintLower : Constraint -> String
constraintLower constraint =
  let
      str = Constraint.toString constraint
  in
    str
      |> String.words
      |> List.head
      |> Maybe.withDefault str


-- LINK HELPERS


navLink : String -> String -> Bool -> Html msg
navLink =
  navLinkHelp "pkg-nav-link"


navModuleLink : String -> String -> Bool -> Html msg
navModuleLink =
  navLinkHelp "pkg-nav-module"


navLinkHelp : String -> String -> String -> Bool -> Html msg
navLinkHelp linkClass name url isBold =
  let
    attributes =
      if isBold then
        [ class linkClass
        , style "font-weight" "bold"
        , style "text-decoration" "underline"
        ]
      else
        [ class linkClass
        ]
  in
  a
    (href url :: id ("nav-" ++ name) :: attributes)
    [ text name ]
