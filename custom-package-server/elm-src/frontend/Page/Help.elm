module Page.Help exposing
  ( Model
  , init
  , Msg
  , update
  , view
  )


import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Markdown
import Session
import Skeleton



-- MODEL


type alias Model =
  { session : Session.Data
  , title : String
  , content : Content
  }


type Content
  = Failure
  | Loading
  | Success String


init : Session.Data -> String -> String -> ( Model, Cmd Msg )
init session title url =
  ( Model session title Loading
  , Http.send GotContent (Http.getString url)
  )



-- UPDATE


type Msg
  = GotContent (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    GotContent result ->
      case result of
        Err _ ->
          ( { model | content = Failure }, Cmd.none )

        Ok content ->
          ( { model | content = Success content }, Cmd.none )



-- VIEW


view : Model -> Skeleton.Details msg
view model =
  { title = model.title
  , header = [Skeleton.helpSegment]
  , warning = Skeleton.NoProblems
  , attrs = []
  , kids = [ viewContent model.title model.content ]
  }


viewContent : String -> Content -> Html msg
viewContent title content =
  div [ class "block-list" ] <|
    case content of
      Failure ->
        [ text "" ]-- TODO

      Loading ->
        [ h1 [] [ text title ] ]

      Success help ->
        [ Markdown.toHtml [] help ]
