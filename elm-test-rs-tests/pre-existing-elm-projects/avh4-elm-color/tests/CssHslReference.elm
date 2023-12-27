module CssHslReference exposing (ColorInfo, all)

{-| HSL RGB conversions from <https://www.w3.org/TR/css-color-3/#hsl-color>
-}

import Array
import CssHslReferenceData as Data
import Hex
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required, requiredAt)


all : List ColorInfo
all =
    case Json.Decode.decodeString (Json.Decode.list decode) Data.json of
        Err err ->
            Debug.todo (Json.Decode.errorToString err)

        Ok info ->
            info


type alias ColorInfo =
    { h : Float
    , s : Float
    , l : Float
    , r : Float
    , g : Float
    , b : Float
    }


decode : Decoder ColorInfo
decode =
    Json.Decode.succeed ColorInfo
        |> required "h" Json.Decode.float
        |> required "s" Json.Decode.float
        |> required "l" Json.Decode.float
        |> required "r" Json.Decode.float
        |> required "g" Json.Decode.float
        |> required "b" Json.Decode.float
