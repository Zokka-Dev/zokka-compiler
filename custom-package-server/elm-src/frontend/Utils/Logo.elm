module Utils.Logo exposing (logo)


import Html
import Svg exposing (..)
import Svg.Attributes as A exposing (..)



-- ELM LOGO


logo : Int -> Html.Html msg
logo n =
  let
    size = String.fromInt n
  in
  svg
    [ width size
    , height size
    , viewBox "0 0 600 600"
    ]
    [ shape "0,20 280,300 0,580"
    , shape "20,600 300,320 580,600"
    , shape "320,0 600,0 600,280"
    , shape "20,0 280,0 402,122 142,122"
    , shape "170,150 430,150 300,280"
    , shape "320,300 450,170 580,300 450,430"
    , shape "470,450 600,320 600,580"
    ]


shape : String -> Svg msg
shape coordinates =
  polygon [ points coordinates ] []
