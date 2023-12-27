module Color exposing
    ( Color
    , rgb255, rgb, rgba, hsl, hsla
    , fromRgba, fromHsla
    , toCssString
    , toRgba, toHsla
    , red, orange, yellow, green, blue, purple, brown
    , lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown
    , darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown
    , white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black
    , lightGray, gray, darkGray
    )

{-| This package defines a standard `Color` type
with the hope that all Elm packages that produce colors and
all Elm packages that consume colors will use this type
to allow all such packages to easily interoperate
for the ultimate benefit of all Elm developers.

Note about color space conversions:
When converting between RGB and HSL, this module produce results consistent with
the algorithm specified in the CSS Color Module Level 3,
[Section 4.2.4. HSL color values](https://www.w3.org/TR/css-color-3/#hsl-color).


# Types

@docs Color


# Creating colors


## From numbers

These are the most concise ways to create colors:

@docs rgb255, rgb, rgba, hsl, hsla


## From records

These ways to make colors make the names of each component explicit,
and are compatible with the corresponding `to...` function.

@docs fromRgba, fromHsla


# Using colors with HTML/CSS/SVG

@docs toCssString


# Extracting values from colors

@docs toRgba, toHsla


# Built-in Colors

These colors come from the [Tango palette](http://tango.freedesktop.org/Tango_Icon_Theme_Guidelines)
which provides aesthetically reasonable defaults for colors.
Each color also comes with a light and dark version.


## Standard

@docs red, orange, yellow, green, blue, purple, brown


## Light

@docs lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown


## Dark

@docs darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown


## Eight Shades of Grey

These colors are a compatible series of shades of grey, fitting nicely
with the Tango palette.

@docs white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black

These are identical to the _grey_ versions. It seems the spelling is regional, but
that has never helped me remember which one I should be writing.

@docs lightGray, gray, darkGray

-}

import Bitwise exposing (shiftLeftBy)


{-| Represents a color.
-}
type Color
    = RgbaSpace Float Float Float Float


{-| Creates a color from a record of RGBA values (red, green, blue, alpha) between 0.0 and 1.0 (inclusive).

The RGB values are interpreted in the [sRGB](https://en.wikipedia.org/wiki/SRGB) color space,
which is the color space specified by the HTML, [CSS](https://www.w3.org/TR/css-color-3/#rgb-color),
and [SVG](https://www.w3.org/Graphics/SVG/1.1/color.html) specs
(and is also widely considered the default color space for digital images that do not explicitly contain color space information).

This is a strict function that will force you to name all channel parameters, to avoid mixing them up.

See also:

  - If you want to be more concise, see [`rgba`](#rgba) or [`rgb`](#rgb).

-}
fromRgba : { red : Float, green : Float, blue : Float, alpha : Float } -> Color
fromRgba components =
    RgbaSpace components.red components.green components.blue components.alpha


{-| Creates a color from RGBA (red, green, blue, alpha) values between 0.0 and 1.0 (inclusive).

See also:

  - If you want to be more concise and want full alpha, see [`rgb`](#rgb).
  - If you want to be more explicit with parameter names, see [`fromRgba`](#fromRgba).

-}
rgba : Float -> Float -> Float -> Float -> Color
rgba r g b a =
    RgbaSpace r g b a


{-| Creates a color from RGB (red, green, blue) values between 0.0 and 1.0 (inclusive).

This is a convenience function for making a color value with full opacity.

See also:

  - If you want to pass RGB values as `Int` values between 0 and 255, see [`rgb255`](#rgb255).
  - If you need to provide an alpha value, see [`rgba`](#rgba).
  - If you want to be more explicit with parameter names, see [`fromRgba`](#fromRgba).

-}
rgb : Float -> Float -> Float -> Color
rgb r g b =
    RgbaSpace r g b 1.0


{-| Creates a color from RGB (red, green, blue) integer values between 0 and 255.

This is a convenience function if you find passing RGB channels as integers scaled to 255 more intuitive.

See also:

  - If you want to provide RGB values as `Float` values between 0.0 and 1.0, see [`rgb`](#rgb).

-}
rgb255 : Int -> Int -> Int -> Color
rgb255 r g b =
    RgbaSpace (scaleFrom255 r) (scaleFrom255 g) (scaleFrom255 b) 1.0


scaleFrom255 : Int -> Float
scaleFrom255 c =
    toFloat c / 255


{-| Creates a color from [HSLA](https://en.wikipedia.org/wiki/HSL_and_HSV) (hue, saturation, lightness, alpha)
values between 0.0 and 1.0 (inclusive).

See also:

  - If you want to be more concise, see [`hsla`](#hsla) or [`hsl`](#hsl).

-}
fromHsla : { hue : Float, saturation : Float, lightness : Float, alpha : Float } -> Color
fromHsla { hue, saturation, lightness, alpha } =
    hsla hue saturation lightness alpha


{-| Creates a color from [HSLA](https://en.wikipedia.org/wiki/HSL_and_HSV) (hue, saturation, lightness, alpha)
values between 0.0 and 1.0 (inclusive).

See also:

  - If you want to be more concise and want full alpha, see [`hsl`](#hsl).
  - If you want to be more explicit with parameter names, see [`fromHsla`](#fromHsla).

-}
hsla : Float -> Float -> Float -> Float -> Color
hsla hue sat light alpha =
    let
        ( h, s, l ) =
            ( hue, sat, light )

        m2 =
            if l <= 0.5 then
                l * (s + 1)

            else
                l + s - l * s

        m1 =
            l * 2 - m2

        r =
            hueToRgb (h + 1 / 3)

        g =
            hueToRgb h

        b =
            hueToRgb (h - 1 / 3)

        hueToRgb h__ =
            let
                h_ =
                    if h__ < 0 then
                        h__ + 1

                    else if h__ > 1 then
                        h__ - 1

                    else
                        h__
            in
            if h_ * 6 < 1 then
                m1 + (m2 - m1) * h_ * 6

            else if h_ * 2 < 1 then
                m2

            else if h_ * 3 < 2 then
                m1 + (m2 - m1) * (2 / 3 - h_) * 6

            else
                m1
    in
    RgbaSpace r g b alpha


{-| Creates a color from [HSL](https://en.wikipedia.org/wiki/HSL_and_HSV) (hue, saturation, lightness)
values between 0.0 and 1.0 (inclusive).

See also:

  - If you need to provide an alpha value, see [`hsla`](#hsla).
  - If you want to be more explicit with parameter names, see [`fromHsla`](#fromHsla).

-}
hsl : Float -> Float -> Float -> Color
hsl h s l =
    hsla h s l 1.0


{-| Extract the [HSLA](https://en.wikipedia.org/wiki/HSL_and_HSV) (hue, saturation, lightness, alpha)
components from a color.
The component values will be between 0.0 and 1.0 (inclusive).
-}
toHsla : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsla (RgbaSpace r g b a) =
    let
        minColor =
            min r (min g b)

        maxColor =
            max r (max g b)

        h1 =
            if maxColor == r then
                (g - b) / (maxColor - minColor)

            else if maxColor == g then
                2 + (b - r) / (maxColor - minColor)

            else
                4 + (r - g) / (maxColor - minColor)

        h2 =
            h1 * (1 / 6)

        h3 =
            if isNaN h2 then
                0

            else if h2 < 0 then
                h2 + 1

            else
                h2

        l =
            (minColor + maxColor) / 2

        s =
            if minColor == maxColor then
                0

            else if l < 0.5 then
                (maxColor - minColor) / (maxColor + minColor)

            else
                (maxColor - minColor) / (2 - maxColor - minColor)
    in
    { hue = h3
    , saturation = s
    , lightness = l
    , alpha = a
    }


{-| Extract the RGBA (red, green, blue, alpha) components from a color.
The component values will be between 0.0 and 1.0 (inclusive).

The RGB values are interpreted in the [sRGB](https://en.wikipedia.org/wiki/SRGB) color space,
which is the color space specified by the HTML, [CSS](https://www.w3.org/TR/css-color-3/#rgb-color),
and [SVG](https://www.w3.org/Graphics/SVG/1.1/color.html) specs
(and is also widely considered the default color space for digital images that do not explicitly contain color space information).

-}
toRgba : Color -> { red : Float, green : Float, blue : Float, alpha : Float }
toRgba (RgbaSpace r g b a) =
    { red = r, green = g, blue = b, alpha = a }


{-| Returns a color represented by a valid 3- or 6-digit RGB hex string
or a 4- or 8-digit RGBA hex string.
String may (but are not required to) start with a `#` character.
Hex digits in the string may be either uppercase or lowercase.

If the input string is not a valid hex string, it will return `Nothing`.

    fromHex "#Ac3" --> Just (Color.rgb255 0xAA 0xCC 0x33)

    fromHex "ffe4e1" --> Just (Color.rgb255 0xFF 0xE4 0xE1)

    fromHex "#00ff00FF" --> Just (Color.rgba 0.0 1.0 0.0 1.0)

    fromHex "**purple**" --> Nothing

-}
fromHex : String -> Maybe Color
fromHex hexString =
    case String.toList hexString of
        [ '#', r, g, b ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( 'f', 'f' )

        [ r, g, b ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( 'f', 'f' )

        [ '#', r, g, b, a ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( a, a )

        [ r, g, b, a ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( a, a )

        [ '#', r1, r2, g1, g2, b1, b2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( 'f', 'f' )

        [ r1, r2, g1, g2, b1, b2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( 'f', 'f' )

        [ '#', r1, r2, g1, g2, b1, b2, a1, a2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 )

        [ r1, r2, g1, g2, b1, b2, a1, a2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 )

        _ ->
            Nothing


fromHex8 : ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> Maybe Color
fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 ) =
    Maybe.map4
        (\r g b a ->
            RgbaSpace
                (toFloat r / 255)
                (toFloat g / 255)
                (toFloat b / 255)
                (toFloat a / 255)
        )
        (hex2ToInt r1 r2)
        (hex2ToInt g1 g2)
        (hex2ToInt b1 b2)
        (hex2ToInt a1 a2)


hex2ToInt : Char -> Char -> Maybe Int
hex2ToInt c1 c2 =
    Maybe.map2 (\v1 v2 -> shiftLeftBy 4 v1 + v2) (hexToInt c1) (hexToInt c2)


hexToInt : Char -> Maybe Int
hexToInt char =
    case Char.toLower char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'a' ->
            Just 10

        'b' ->
            Just 11

        'c' ->
            Just 12

        'd' ->
            Just 13

        'e' ->
            Just 14

        'f' ->
            Just 15

        _ ->
            Nothing


{-| This function will convert a color to a 6-digit hexadecimal string in the format `#rrggbb`.

NOTE: If you want to use the resulting string with CSS, you should instead use [`toCssString`](#toCssString),
which will represent the color more accurately, and preserve the alpha component.

-}
toHex : Color -> { hex : String, alpha : Float }
toHex c =
    let
        components =
            toRgba c
    in
    { hex =
        [ components.red, components.green, components.blue ]
            |> List.map ((*) 255)
            |> List.map round
            |> List.map int255ToHex
            |> String.concat
            |> (++) "#"
    , alpha = components.alpha
    }


int255ToHex : Int -> String
int255ToHex n =
    if n < 0 then
        "00"

    else if n > 255 then
        "ff"

    else
        unsafeInt255Digits n
            |> Tuple.mapBoth unsafeIntToChar unsafeIntToChar
            |> (\( a, b ) -> String.cons a (String.cons b ""))


unsafeInt255Digits : Int -> ( Int, Int )
unsafeInt255Digits n =
    let
        digit1 =
            n // 16

        digit0 =
            if digit1 /= 0 then
                modBy (digit1 * 16) n

            else
                n
    in
    ( digit1, digit0 )


unsafeIntToChar : Int -> Char
unsafeIntToChar i =
    if i < 10 then
        String.fromInt i
            |> String.uncons
            |> Maybe.map Tuple.first
            |> Maybe.withDefault '0'

    else
        case i of
            10 ->
                'a'

            11 ->
                'b'

            12 ->
                'c'

            13 ->
                'd'

            14 ->
                'e'

            15 ->
                'f'

            _ ->
                '0'


{-| Converts a color to a string suitable for use in CSS.
The string will conform to [CSS Color Module Level 3](https://www.w3.org/TR/css-color-3/),
which is supported by all current web browsers, all versions of Firefox,
all versions of Chrome, IE 9+, and all common mobile browsers
([browser support details](https://caniuse.com/#feat=css3-colors)).

    Html.Attributes.style "background-color" (Color.toCssString Color.lightPurple)

Note: the current implementation produces a string in the form
`rgba(rr.rr%,gg.gg%,bb.bb%,a.aaa)`, but this may change in the
future, and you should not rely on this implementation detail.

-}
toCssString : Color -> String
toCssString (RgbaSpace r g b a) =
    let
        pct x =
            ((x * 10000) |> round |> toFloat) / 100

        roundTo x =
            ((x * 1000) |> round |> toFloat) / 1000
    in
    "rgba("
        ++ String.fromFloat (pct r)
        ++ "%,"
        ++ String.fromFloat (pct g)
        ++ "%,"
        ++ String.fromFloat (pct b)
        ++ "%,"
        ++ String.fromFloat (roundTo a)
        ++ ")"



--
-- Built-in colors
--


{-| -}
lightRed : Color
lightRed =
    RgbaSpace (239 / 255) (41 / 255) (41 / 255) 1.0


{-| -}
red : Color
red =
    RgbaSpace (204 / 255) (0 / 255) (0 / 255) 1.0


{-| -}
darkRed : Color
darkRed =
    RgbaSpace (164 / 255) (0 / 255) (0 / 255) 1.0


{-| -}
lightOrange : Color
lightOrange =
    RgbaSpace (252 / 255) (175 / 255) (62 / 255) 1.0


{-| -}
orange : Color
orange =
    RgbaSpace (245 / 255) (121 / 255) (0 / 255) 1.0


{-| -}
darkOrange : Color
darkOrange =
    RgbaSpace (206 / 255) (92 / 255) (0 / 255) 1.0


{-| -}
lightYellow : Color
lightYellow =
    RgbaSpace (255 / 255) (233 / 255) (79 / 255) 1.0


{-| -}
yellow : Color
yellow =
    RgbaSpace (237 / 255) (212 / 255) (0 / 255) 1.0


{-| -}
darkYellow : Color
darkYellow =
    RgbaSpace (196 / 255) (160 / 255) (0 / 255) 1.0


{-| -}
lightGreen : Color
lightGreen =
    RgbaSpace (138 / 255) (226 / 255) (52 / 255) 1.0


{-| -}
green : Color
green =
    RgbaSpace (115 / 255) (210 / 255) (22 / 255) 1.0


{-| -}
darkGreen : Color
darkGreen =
    RgbaSpace (78 / 255) (154 / 255) (6 / 255) 1.0


{-| -}
lightBlue : Color
lightBlue =
    RgbaSpace (114 / 255) (159 / 255) (207 / 255) 1.0


{-| -}
blue : Color
blue =
    RgbaSpace (52 / 255) (101 / 255) (164 / 255) 1.0


{-| -}
darkBlue : Color
darkBlue =
    RgbaSpace (32 / 255) (74 / 255) (135 / 255) 1.0


{-| -}
lightPurple : Color
lightPurple =
    RgbaSpace (173 / 255) (127 / 255) (168 / 255) 1.0


{-| -}
purple : Color
purple =
    RgbaSpace (117 / 255) (80 / 255) (123 / 255) 1.0


{-| -}
darkPurple : Color
darkPurple =
    RgbaSpace (92 / 255) (53 / 255) (102 / 255) 1.0


{-| -}
lightBrown : Color
lightBrown =
    RgbaSpace (233 / 255) (185 / 255) (110 / 255) 1.0


{-| -}
brown : Color
brown =
    RgbaSpace (193 / 255) (125 / 255) (17 / 255) 1.0


{-| -}
darkBrown : Color
darkBrown =
    RgbaSpace (143 / 255) (89 / 255) (2 / 255) 1.0


{-| -}
black : Color
black =
    RgbaSpace (0 / 255) (0 / 255) (0 / 255) 1.0


{-| -}
white : Color
white =
    RgbaSpace (255 / 255) (255 / 255) (255 / 255) 1.0


{-| -}
lightGrey : Color
lightGrey =
    RgbaSpace (238 / 255) (238 / 255) (236 / 255) 1.0


{-| -}
grey : Color
grey =
    RgbaSpace (211 / 255) (215 / 255) (207 / 255) 1.0


{-| -}
darkGrey : Color
darkGrey =
    RgbaSpace (186 / 255) (189 / 255) (182 / 255) 1.0


{-| -}
lightGray : Color
lightGray =
    RgbaSpace (238 / 255) (238 / 255) (236 / 255) 1.0


{-| -}
gray : Color
gray =
    RgbaSpace (211 / 255) (215 / 255) (207 / 255) 1.0


{-| -}
darkGray : Color
darkGray =
    RgbaSpace (186 / 255) (189 / 255) (182 / 255) 1.0


{-| -}
lightCharcoal : Color
lightCharcoal =
    RgbaSpace (136 / 255) (138 / 255) (133 / 255) 1.0


{-| -}
charcoal : Color
charcoal =
    RgbaSpace (85 / 255) (87 / 255) (83 / 255) 1.0


{-| -}
darkCharcoal : Color
darkCharcoal =
    RgbaSpace (46 / 255) (52 / 255) (54 / 255) 1.0
