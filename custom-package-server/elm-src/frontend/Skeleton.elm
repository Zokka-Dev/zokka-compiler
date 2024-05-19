module Skeleton exposing
  ( Details
  , Warning(..)
  , view
  , Segment
  , helpSegment
  , authorSegment
  , projectSegment
  , versionSegment
  , moduleSegment
  )


import Browser
import Elm.Version as V
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Href
import Json.Decode as D
import Utils.Logo as Logo



-- NODE


type alias Details msg =
  { title : String
  , header : List Segment
  , warning : Warning
  , attrs : List (Attribute msg)
  , kids : List (Html msg)
  }


type Warning
  = NoProblems
  | NewerVersion String V.Version
  | IncompatibleElm V.Version



-- SEGMENT


type Segment
  = Text String
  | Link String String


helpSegment : Segment
helpSegment =
  Text "help"


authorSegment : String -> String -> Segment
authorSegment repository author =
  Link (Href.toAuthor repository author) author


projectSegment : String -> String -> String -> Segment
projectSegment repository author project =
  Link (Href.toProject repository author project) project


versionSegment : String -> String -> String -> Maybe V.Version -> Segment
versionSegment repository author project version =
  Link (Href.toVersion repository author project version Nothing) (vsnToString version)


moduleSegment : String -> String -> String -> Maybe V.Version -> String -> Segment
moduleSegment repository author project version moduleName =
  Link (Href.toModule repository author project version moduleName Nothing) moduleName


vsnToString : Maybe V.Version -> String
vsnToString maybeVersion =
  case maybeVersion of
    Nothing ->
      "latest"

    Just version ->
      V.toString version



-- VIEW


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
  { title =
      details.title
  , body =
      [ div [ class "page" ]
        [ div [ class "page-margin" ] []
        , div [ class "content" ]
            [ viewHeader details.header
            , lazy viewWarning details.warning
            , Html.map toMsg <|
                div (class "center" :: details.attrs) details.kids
            ]
        , div [ class "page-margin" ] []
        ]
      , viewFooter
      ]
  }



-- VIEW HEADER


viewHeader : List Segment -> Html msg
viewHeader segments =
  div
    [ class "header-bar"
    ]
    [ div [class "center"]
        [ h1 [ class "header" ] <|
            viewLogo :: List.intersperse slash (List.map viewSegment segments)
        ]
    ]



slash : Html msg
slash =
  span [ class "spacey-char" ] [ text "/" ]


viewSegment : Segment -> Html msg
viewSegment segment =
  case segment of
    Text string ->
      text string

    Link address string ->
      a [ href address ] [ text string ]



-- VIEW WARNING


viewWarning : Warning -> Html msg
viewWarning warning =
  case warning of
    NoProblems ->
      text ""

    NewerVersion url version ->
      p [ class "version-warning" ]
        [ text "Warning! The latest version of this package is "
        , a [ href url ] [ text (V.toString version) ]
        ]

    IncompatibleElm version ->
      p [ class "version-warning" ]
        [ text ("Warning! This package is not compatible with Elm " ++ V.toString version)
        ]




-- VIEW FOOTER


viewFooter : Html msg
viewFooter =
  div [class "footer"]
    [ a
        [ class "grey-link", href "https://github.com/dmy/elm.dmy.fr/" ]
        [ text "Elm © 2020" ]
    ]



-- VIEW LOGO


viewLogo : Html msg
viewLogo =
  a [ href "/"
    , style "text-decoration" "none"
    ]
    [
      div
        [ style "display" "-webkit-display"
        , style "display" "-ms-flexbox"
        , style "display" "flex"
        ]
        [ div
            [ class "logo-text"
            , style "padding-left" "8px"
            ]
            [ div
                [ style "line-height" "20px"
                , style "font-size" "30px"
                , style "transform" "translateY(-4px)"
                ]
                [ text "zokka custom repository" ]
            , div
                [ style "line-height" "10px"
                , style "font-size" "12px"
                , style "font-weight" "600"
                ]
                [ text "packages" ]
            ]
        ]
    ]
