module TCOProducesBadClosures exposing (..)

import Html
import Debug

makeLazy : List a -> List (() -> a) -> List (() -> a)
makeLazy list accum =
    case list of
        item :: items ->
            makeLazy items <| ((\_ -> item) :: accum)

        _ ->
            accum

tcoMakeLazy : List a -> List (() -> a) -> List (() -> a)
tcoMakeLazy list accum =
    case list of
        item :: items ->
            tcoMakeLazy items ((\_ -> item) :: accum)

        _ ->
            accum

-- [1, 2, 3]
goodOutput =
    makeLazy [ 1, 2, 3 ] [] |> List.map (\f -> f ())

-- [3, 3, 3]
badOutput =
    tcoMakeLazy [ 1, 2, 3 ] [] |> List.map (\f -> f ())


main = Html.text (Debug.toString goodOutput ++ Debug.toString badOutput)
