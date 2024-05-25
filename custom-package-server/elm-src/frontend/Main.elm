module Main exposing (..)


import Browser
import Browser.Navigation as Nav
import Browser.Events
import Dict
import Elm.Version as V
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode
import Page.Docs as Docs
import Page.Diff as Diff
import Page.Help as Help
import Page.Problem as Problem
import Page.Search as Search
import Session
import Skeleton
import Task
import Url
import Url.Builder
import Url.Parser as Parser exposing (Parser, (</>), (<?>), custom, fragment, map, oneOf, s, top)
import Url.Parser.Query as Query
import Utils.HttpHelpers exposing (httpPostIgnoreResponseBody)
import Utils.LoginUpdate exposing (LoginUpdate(..))
import Utils.Source as Source



-- MAIN


main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }



-- MODEL

type alias LoginPageInfo =
    -- First set of fields are for creating a new user
    { usernameToCreate : Maybe String
    , passwordToCreate : Maybe String
    -- Ask the user to repeat the password to make sure they match
    , passwordConfirmation : Maybe String
    -- Second set of fields are for logging in as a pre-existing user
    , loginUsername : Maybe String
    , loginPassword : Maybe String
    }

emptyLoginPageInfo : LoginPageInfo
emptyLoginPageInfo =
    { usernameToCreate = Nothing
    , passwordToCreate = Nothing
    , passwordConfirmation = Nothing
    , loginUsername = Nothing
    , loginPassword = Nothing
    }

type LoginStatus = LoggedIn | NotLoggedIn LoginPageInfo | LoginFailed LoginPageInfo | UnknownLoginStatus | LoggedInButUnauthorized

type alias Model =
  { key : Nav.Key
  , page : Page
  , width : Int
  , loginStatus : LoginStatus
  }


type Page
  = NotFound Session.Data
  | Search Search.Model
  | Docs Docs.Model
  | Diff Diff.Model
  | Help Help.Model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Browser.Events.onResize WindowResized



-- VIEW


view : Model -> Browser.Document Msg
view model =
  let
    pageView =
      case model.page of
        NotFound _ ->
          Skeleton.view never
            { title = "Not Found"
            , header = []
            , warning = Skeleton.NoProblems
            , attrs = Problem.styles
            , kids = Problem.notFound
            }

        Search search ->
          Skeleton.view SearchMsg (Search.view search)

        Docs docs ->
          Skeleton.view DocsMsg (Docs.view docs model.width)

        Diff diff ->
          Skeleton.view never (Diff.view diff)

        Help help ->
          Skeleton.view never (Help.view help)
  in
  case Debug.log "top-level view" model.loginStatus of
      LoggedInButUnauthorized -> { title = "Unauthorized", body = [ Html.text "You are not authorized to see this page!" ] }
      NotLoggedIn loginPageInfo -> { title = "Login Page", body = [ viewLoginPage loginPageInfo ] }
      LoginFailed loginPageInfo -> { title = "Login Page", body = [ viewIncorrectLoginPage loginPageInfo ] }
      LoggedIn -> pageView
      UnknownLoginStatus -> pageView


viewLoginPage : LoginPageInfo -> Html.Html Msg
viewLoginPage loginPageInfo =
  Html.div []
    [ Html.text "You must login!"
    , loginForm loginPageInfo
    , Html.text "Or you can create a new user!"
    , userCreationForm loginPageInfo
    ]

viewIncorrectLoginPage : LoginPageInfo -> Html.Html Msg
viewIncorrectLoginPage loginPageInfo =
  Html.div []
  -- Need a better message here than this
    [ Html.text "Something went wrong with your login or user creation request. Did you login with incorrect credentials or try to create a user with the same username as someone else?"
    , loginForm loginPageInfo
    , Html.text "Or you can create a new user!"
    , userCreationForm loginPageInfo
    ]


loginForm : LoginPageInfo -> Html.Html Msg
loginForm loginPageInfo =
  Html.form
    -- Figure out some way to get rid of the withDefault with a top-level case match
    [ Html.Events.onSubmit (SubmitUserLoginRequest { username = Maybe.withDefault "" loginPageInfo.loginUsername, password = Maybe.withDefault "" loginPageInfo.loginPassword }) ]
    [ Html.div
      []
      [ Html.label [ Html.Attributes.for "username" ] [ Html.text "Username:" ]
      , Html.input [ Html.Attributes.type_ "text", Html.Attributes.id "login-username", Html.Events.onInput UserLoginUsernameChanged, Html.Attributes.value (Maybe.withDefault "" loginPageInfo.loginUsername) ] []
      ]
    , Html.div
      []
      [ Html.label [ Html.Attributes.for "password" ] [ Html.text "Password:" ]
      , Html.input [ Html.Attributes.type_ "text", Html.Attributes.id "login-password", Html.Events.onInput UserLoginPasswordChanged, Html.Attributes.value (Maybe.withDefault "" loginPageInfo.loginPassword) ] []
      ]
    , Html.div
      []
      [ Html.button [ Html.Attributes.type_ "submit" ] [ Html.text "Submit" ] ]
    ]

userCreationForm : LoginPageInfo -> Html.Html Msg
userCreationForm loginPageInfo =
  Html.form
    -- Figure out some way to get rid of the withDefault with a top-level case match
    [ Html.Events.onSubmit (SubmitUserCreationRequest { username = Maybe.withDefault "" loginPageInfo.usernameToCreate, password = Maybe.withDefault "" loginPageInfo.passwordToCreate }) ]
    [ Html.div
      []
      [ Html.label [ Html.Attributes.for "username" ] [ Html.text "Username:" ]
      , Html.input [ Html.Attributes.type_ "text", Html.Attributes.id "creation-username", Html.Events.onInput UserCreationUsernameChanged, Html.Attributes.value (Maybe.withDefault "" loginPageInfo.usernameToCreate) ] []
      ]
    , Html.div
      []
      [ Html.label [ Html.Attributes.for "password" ] [ Html.text "Password:" ]
      , Html.input [ Html.Attributes.type_ "text", Html.Attributes.id "creation-password", Html.Events.onInput UserCreationPasswordChanged, Html.Attributes.value (Maybe.withDefault "" loginPageInfo.passwordToCreate) ] []
      ]
    , Html.div
      []
      [ Html.button [ Html.Attributes.type_ "submit" ] [ Html.text "Submit" ] ]
    ]

-- INIT


init : Int -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init width url key =
  stepUrl url
    { key = key
    , page = NotFound Session.empty
    , width = width
    , loginStatus = UnknownLoginStatus
    }



-- UPDATE


type Msg
  = NoOp
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | SearchMsg Search.Msg
  | DiffMsg Diff.Msg
  | DocsMsg Docs.Msg
  | HelpMsg Help.Msg
  | WindowResized Int Int
  | GotModuleSource String String String String String (Result Http.Error String)
  | SubmitUserCreationRequest { username : String, password : String }
  | SubmitUserLoginRequest { username : String, password : String }
  | ReceivedUserCreationConfirmation (Result Http.Error ())
  | ReceivedUserLoginConfirmation (Result Http.Error ())
  | UserCreationUsernameChanged String
  | UserCreationPasswordChanged String
  | UserLoginUsernameChanged String
  | UserLoginPasswordChanged String



update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
  let
      blahblah = Debug.log "Is this a model?" model
  in
  case message of
    NoOp ->
      ( model, Cmd.none )

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model
          , Nav.pushUrl model.key (Url.toString url)
          )

        Browser.External href ->
          ( model
          , Nav.load href
          )

    UrlChanged url ->
      stepUrl url model

    SearchMsg msg ->
      case model.page of
        Search search -> stepSearch model (Search.update model.key msg search)
        _             -> ( model, Cmd.none )

    DiffMsg msg ->
      case model.page of
        Diff diff -> stepDiff model (Diff.update msg diff)
        _         -> ( model, Cmd.none )

    DocsMsg msg ->
      case model.page of
        Docs docs -> stepDocs model (Docs.update msg docs)
        _         -> ( model, Cmd.none )

    HelpMsg msg ->
      case model.page of
        Help docs -> stepHelp model (Help.update msg docs)
        _         -> ( model, Cmd.none )


    WindowResized width height ->
      ( { model | width = width }
      , Cmd.none
      )

    GotModuleSource author project version moduleName tag result ->
      case result of
        Ok code ->
          ( model
          , loadGithubSource author project version moduleName tag code
          )

        Err _ ->
          ( model
          , Cmd.none
          )

    SubmitUserLoginRequest userLoginInfo ->
      ( model
      , Http.send ReceivedUserLoginConfirmation <|
        httpPostIgnoreResponseBody
          "/dashboard/login"
          (
            Http.multipartBody
              [ Http.stringPart "username" userLoginInfo.username
              , Http.stringPart "password" userLoginInfo.password
              ]
          )
          ()
      )


    SubmitUserCreationRequest userCreationInfo ->
      ( model
      , Http.send ReceivedUserCreationConfirmation <|
        httpPostIgnoreResponseBody
          "/dashboard/user"
          (
            Http.multipartBody
              [ Http.stringPart "username" userCreationInfo.username
              , Http.stringPart "password" userCreationInfo.password
              ]
          )
          ()
      )


    ReceivedUserCreationConfirmation httpResponse ->
      case httpResponse of
        -- Deal with this error in a better way
        Err _ -> ({ model | loginStatus = LoginFailed emptyLoginPageInfo }, Cmd.none)
        Ok () -> ({ model | loginStatus = LoggedIn }, Cmd.none)


    ReceivedUserLoginConfirmation httpResponse ->
      case Debug.log "httpResponse for login confirmation" httpResponse of
        -- Deal with this error in a better way
        Err _ -> ({ model | loginStatus = LoginFailed emptyLoginPageInfo }, Cmd.none)
        Ok () -> ({ model | loginStatus = LoggedIn }, Cmd.none)

    UserCreationUsernameChanged string ->
      let
        newLoginStatus =
          case model.loginStatus of
              LoggedIn -> LoggedIn
              NotLoggedIn loginPageInfo -> NotLoggedIn { loginPageInfo | usernameToCreate = Just string }
              LoginFailed loginPageInfo -> LoginFailed { loginPageInfo | usernameToCreate = Just string }
              UnknownLoginStatus -> UnknownLoginStatus
              LoggedInButUnauthorized -> LoggedInButUnauthorized
      in
      ({ model | loginStatus = newLoginStatus }, Cmd.none)


    UserCreationPasswordChanged string ->
      let
        newLoginStatus =
          case model.loginStatus of
              LoggedIn -> LoggedIn
              NotLoggedIn loginPageInfo -> NotLoggedIn { loginPageInfo | passwordToCreate = Just string }
              LoginFailed loginPageInfo -> LoginFailed { loginPageInfo | passwordToCreate = Just string }
              UnknownLoginStatus -> UnknownLoginStatus
              LoggedInButUnauthorized -> LoggedInButUnauthorized
      in
      ({ model | loginStatus = newLoginStatus }, Cmd.none)


    UserLoginUsernameChanged string ->
      let
        newLoginStatus =
          case model.loginStatus of
              LoggedIn -> LoggedIn
              NotLoggedIn loginPageInfo -> NotLoggedIn { loginPageInfo | loginUsername = Just string }
              LoginFailed loginPageInfo -> LoginFailed { loginPageInfo | loginUsername = Just string }
              UnknownLoginStatus -> UnknownLoginStatus
              LoggedInButUnauthorized -> LoggedInButUnauthorized
      in
      ({ model | loginStatus = newLoginStatus }, Cmd.none)


    UserLoginPasswordChanged string ->
      let
        newLoginStatus =
          case model.loginStatus of
              LoggedIn -> LoggedIn
              NotLoggedIn loginPageInfo -> NotLoggedIn { loginPageInfo | loginPassword = Just string }
              LoginFailed loginPageInfo -> LoginFailed { loginPageInfo | loginPassword = Just string }
              UnknownLoginStatus -> UnknownLoginStatus
              LoggedInButUnauthorized -> LoggedInButUnauthorized
      in
      ({ model | loginStatus = newLoginStatus }, Cmd.none)


updateModelBasedOnLoginUpdate : LoginUpdate -> Model -> Model
updateModelBasedOnLoginUpdate loginUpdate model =
    case loginUpdate of
        ConfirmedUserIsLoggedIn -> { model | loginStatus = LoggedIn }
        ConfirmedUserIsNotLoggedIn -> { model | loginStatus = NotLoggedIn emptyLoginPageInfo }
        ConfirmedUserIsLoggedInButWithWrongCreds -> { model | loginStatus = LoggedInButUnauthorized }
        NoUpdateAboutLoginStatus -> model


stepSearch : Model -> ( Search.Model, Cmd Search.Msg, LoginUpdate ) -> ( Model, Cmd Msg )
stepSearch model (search, cmds, loginUpdate) =
  ( { model | page = Search search }
    |> updateModelBasedOnLoginUpdate loginUpdate
  , Cmd.map SearchMsg cmds
  )


stepDocs : Model -> ( Docs.Model, Cmd Docs.Msg, LoginUpdate ) -> ( Model, Cmd Msg )
stepDocs model (docs, cmds, loginUpdate) =
  ( { model | page = Docs docs }
    |> updateModelBasedOnLoginUpdate loginUpdate
  , Cmd.map DocsMsg cmds
  )


stepDiff : Model -> ( Diff.Model, Cmd Diff.Msg, LoginUpdate ) -> ( Model, Cmd Msg )
stepDiff model (diff, cmds, loginUpdate) =
  ( { model | page = Diff diff }
    |> updateModelBasedOnLoginUpdate loginUpdate
  , Cmd.map DiffMsg cmds
  )


stepHelp : Model -> ( Help.Model, Cmd Help.Msg, LoginUpdate ) -> ( Model, Cmd Msg )
stepHelp model (help, cmds, loginUpdate) =
  ( { model | page = Help help }
    |> updateModelBasedOnLoginUpdate loginUpdate
  , Cmd.map HelpMsg cmds
  )



-- GITHUB SOURCE


findGithubSource : Session.Data -> String -> String -> Maybe V.Version -> String -> Maybe String -> Cmd Msg
findGithubSource data author project maybeVersion moduleName maybeTag =
  let
    semanticVersion =
      Maybe.map V.toString <|
        case maybeVersion of
          Just version -> Just version
          Nothing -> Session.getLatestVersion data author project

    modulePath =
      String.replace "." "/" moduleName
  in
  case (semanticVersion, maybeTag) of
    (Just version, Just tag) ->
      Http.send (GotModuleSource author project version modulePath tag) <|
        Http.getString <|
          Url.Builder.crossOrigin "https://raw.githubusercontent.com" 
            [ author , project , version , "src" , modulePath ++ ".elm" ]
            []

    _ ->
      Cmd.none


loadGithubSource : String -> String -> String -> String -> String -> String -> Cmd msg
loadGithubSource author project version moduleName tag code =
  let
    lineNumber =
      Source.findLine tag code
        |> Maybe.withDefault 1 
        |> String.fromInt
  in
  Nav.load <|
    Url.Builder.custom (Url.Builder.CrossOrigin "https://github.com")
      [ author
      , project
      , "blob"
      , version
      , "src"
      , moduleName ++ ".elm"
      ]
      []
      (Just ("L" ++ lineNumber))



-- EXIT


exit : Model -> Session.Data
exit model =
  case model.page of
    NotFound session -> session
    Search m -> m.session
    Docs m -> m.session
    Diff m -> m.session
    Help m -> m.session



-- ROUTER


stepUrl : Url.Url -> Model -> (Model, Cmd Msg)
stepUrl url model =
  let
    session =
      exit model

    hello = (s "packages" </> author_ </> project_ </> version_ </> focus_ <?> source_ <?> Query.string "q")

    parser =
      oneOf
        [ route (top <?> Query.string "q")
            (\query ->
              stepSearch model (Search.init session (Maybe.map (String.replace "+" " ") query ) Nothing Nothing)
            )
        , route (s "repository" </> repository_ </> s "packages" </> author_ <?> Query.string "q")
            (\repository author query ->
              stepSearch model (Search.init session (Maybe.map (String.replace "+" " ") query ) (Just repository) (Just author))
            )
        , route (s "repository" </> repository_ </> s "packages" </> author_ </> project_)
            (\repository author project ->
                stepDiff model (Diff.init session repository author project)
            )
        , route (s "repository" </> repository_ </> s "packages" </> author_ </> project_ </> version_ </> focus_ <?> source_ <?> Query.string "q")
            (\repository author project version focus source query ->
                case (focus, source) of
                  (Docs.Module moduleName tag, Just _) ->
                   let
                     (newModel, cmd) =
                       stepDocs model (Docs.init session repository author project version focus query)
                   in
                     ( newModel
                     , Cmd.batch
                         [ Nav.replaceUrl model.key (Url.toString { url | query = Nothing })
                         --FIXME: Remove GitHub source link
                         , findGithubSource session author project version moduleName tag
                         ]
                     )

                  _ ->
                    stepDocs model (Docs.init session repository author project version focus query)
            )
        , route (s "help" </> s "design-guidelines")
            (stepHelp model (Help.init session "Design Guidelines" "/assets/help/design-guidelines.md"))
        , route (s "help" </> s "documentation-format")
            (stepHelp model (Help.init session "Documentation Format" "/assets/help/documentation-format.md"))
        ]
  in
  case Parser.parse parser (Debug.log "url" url) of
    Just answer ->
      answer

    Nothing ->
      ( { model | page = NotFound session }
      , Cmd.none
      )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
  Parser.map handler parser


repository_ : Parser (String -> a) a
repository_ =
  custom "REPOSITORY" Just


author_ : Parser (String -> a) a
author_ =
  custom "AUTHOR" Just


project_ : Parser (String -> a) a
project_ =
  custom "PROJECT" Just


version_ : Parser (Maybe V.Version -> a) a
version_ =
  custom "VERSION" <| \string ->
    if string == "latest" then
      Just Nothing
    else
      Maybe.map Just (V.fromString string)


module_ : Parser (String -> a) a
module_ =
  custom "MODULE" Just


focus_ : Parser (Docs.Focus -> a) a
focus_ =
  oneOf
    [ map Docs.Readme (top  </> fragment (Maybe.andThen Url.percentDecode))
    , map Docs.About (s "about")
    , map Docs.Module (moduleName_ </> fragment (Maybe.andThen Url.percentDecode))
    ]


source_ : Query.Parser (Maybe String)
source_ =
  Query.string "source"


moduleName_ : Parser (String -> a) a
moduleName_ =
  custom "MODULE" (Just << String.replace "-" ".")
