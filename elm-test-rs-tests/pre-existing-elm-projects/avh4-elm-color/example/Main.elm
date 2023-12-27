module Main exposing (main)

import Browser
import Color exposing (Color)
import CssHslReference
import Html exposing (Html)
import Html.Attributes


main =
    Browser.sandbox
        { init = ()
        , update = \() () -> ()
        , view = \() -> view
        }


view : Html msg
view =
    Html.div
        []
        [ Html.h2 [] [ Html.text "Built-in colors" ]
        , builtInColors
        , Html.h2 [] [ Html.text "HSL reference" ]
        , hslReference
        ]


builtInColors : Html msg
builtInColors =
    [ ( "lightRed", Color.lightRed )
    , ( "lightOrange", Color.lightOrange )
    , ( "lightYellow", Color.lightYellow )
    , ( "lightGreen", Color.lightGreen )
    , ( "lightBlue", Color.lightBlue )
    , ( "lightPurple", Color.lightPurple )
    , ( "lightBrown", Color.lightBrown )
    , ( "red", Color.red )
    , ( "orange", Color.orange )
    , ( "yellow", Color.yellow )
    , ( "green", Color.green )
    , ( "blue", Color.blue )
    , ( "purple", Color.purple )
    , ( "brown", Color.brown )
    , ( "darkRed", Color.darkRed )
    , ( "darkOrange", Color.darkOrange )
    , ( "darkYellow", Color.darkYellow )
    , ( "darkGreen", Color.darkGreen )
    , ( "darkBlue", Color.darkBlue )
    , ( "darkPurple", Color.darkPurple )
    , ( "darkBrown", Color.darkBrown )
    , ( "white", Color.white )
    , ( "lightGrey", Color.lightGrey )
    , ( "grey", Color.grey )
    , ( "darkGrey", Color.darkGrey )
    , ( "lightCharcoal", Color.lightCharcoal )
    , ( "charcoal", Color.charcoal )
    , ( "darkCharcoal", Color.darkCharcoal )
    , ( "black", Color.black )
    ]
        |> List.map (\( name, color ) -> colorExample name color)
        |> Html.div []


hslReference : Html msg
hslReference =
    CssHslReference.all
        |> List.concatMap
            (\color ->
                [ colorExample
                    (String.join " "
                        [ "rgb"
                        , String.fromFloat color.r
                        , String.fromFloat color.g
                        , String.fromFloat color.b
                        ]
                    )
                    (Color.rgb color.r color.g color.b)
                , colorExample
                    (String.join " "
                        [ "hsl"
                        , String.fromFloat color.h
                        , String.fromFloat color.s
                        , String.fromFloat color.l
                        ]
                    )
                    (Color.hsl color.h color.s color.l)
                , Html.br [] []
                ]
            )
        |> Html.div []


colorExample : String -> Color -> Html msg
colorExample label color =
    Html.div
        [ Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "border-radius" "5px"
        , Html.Attributes.style "background" (Color.toCssString color)
        , Html.Attributes.style "text-shadow" "0 0 2px white"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "width" "140px"
        , Html.Attributes.style "height" "70px"
        , Html.Attributes.style "padding" "15px"
        , Html.Attributes.style "box-sizing" "border-box"
        , Html.Attributes.style "margin" "10px"
        , Html.Attributes.style "vertical-align" "bottom"
        ]
        [ Html.text label
        ]
