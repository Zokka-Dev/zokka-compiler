module Page.Diff exposing
  ( Model
  , init
  , Msg
  , update
  , view
  )


import Elm.Version as V
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Http
import Href
import Page.Problem as Problem
import Release
import Session
import Skeleton
import Url.Builder as Url
import Utils.LoginUpdate exposing (LoginUpdate(..), httpErrorToLoginUpdate)
import Utils.Markdown as Markdown
import Utils.OneOrMore as OneOrMore exposing (OneOrMore(..))
import Time



-- MODEL


type alias Model =
  { session : Session.Data
  , repository : String
  , author : String
  , project : String
  , releases : Releases
  }


type Releases
  = Failure
  | Loading
  | Success (OneOrMore Release.Release)


init : Session.Data -> String -> String -> String -> ( Model, Cmd Msg, LoginUpdate )
init session repository author project =
  case Session.getReleases session author project of
    Just releases ->
      ( Model session repository author project (Success releases)
      , Cmd.none
      , NoUpdateAboutLoginStatus
      )

    Nothing ->
      ( Model session repository author project Loading
      , Http.send GotReleases (Session.fetchReleases repository author project)
      , NoUpdateAboutLoginStatus
      )


-- UPDATE


type Msg
  = GotReleases (Result Http.Error (OneOrMore Release.Release))


update : Msg -> Model -> ( Model, Cmd msg, LoginUpdate )
update msg model =
  case msg of
    GotReleases result ->
      case result of
        Err httpError ->
          ( { model | releases = Failure }
          , Cmd.none
          , httpErrorToLoginUpdate httpError
          )

        Ok releases ->
          ( { model
                | releases = Success releases
                , session = Session.addReleases model.author model.project releases model.session
            }
          , Cmd.none
          , ConfirmedUserIsLoggedIn
          )



-- VIEW


view : Model -> Skeleton.Details msg
view model =
  { title = model.author ++ "/" ++ model.project
  , header =
      [ Skeleton.authorSegment model.repository model.author
      , Skeleton.projectSegment model.repository model.author model.project
      ]
  , warning = Skeleton.NoProblems
  , attrs = [ class "pkg-overview" ]
  , kids =
      case model.releases of
        Failure ->
          [ div Problem.styles (Problem.offline "releases.json")
          ]

        Loading ->
          [ text "" -- TODO
          ]

        Success (OneOrMore r rs) ->
          [ h1 [] [ text "Published Versions" ]
          , p [] <|
              viewReleases model.repository model.author model.project <|
                List.map .version (List.sortBy (.time >> Time.posixToMillis) (r::rs))
          ]
  }



viewReleases : String -> String -> String -> List V.Version -> List (Html msg)
viewReleases repository author project versions =
  case versions of
    v1 :: ((v2 :: _) as vs) ->
      let
        attrs =
          if isSameMajor v1 v2 then [] else [ bold ]
      in
      viewReadmeLink repository author project v1 attrs :: text ", " :: viewReleases repository author project vs

    r0 :: [] ->
      [ viewReadmeLink repository author project r0 [ bold ] ]

    [] ->
      []


bold : Attribute msg
bold =
  style "font-weight" "bold"


viewReadmeLink : String -> String -> String -> V.Version -> List (Attribute msg) -> Html msg
viewReadmeLink repository author project version attrs =
  let
    url =
      Href.toVersion repository author project (Just version) Nothing
  in
  a (href url :: attrs) [ text (V.toString version) ]


isSameMajor : V.Version -> V.Version -> Bool
isSameMajor v1 v2 =
  let
    (major1,_,_) = V.toTuple v1
    (major2,_,_) = V.toTuple v2
  in
  major1 == major2
