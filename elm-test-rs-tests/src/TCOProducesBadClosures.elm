module TCOProducesBadClosures exposing (tcoProducesBadClosuresTest)

-- From https://github.com/elm/compiler/issues/2268

import Test exposing (Test, test)
import Expect

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
        itemEscape :: items ->
            tcoMakeLazy items ((\_ -> itemEscape) :: accum)

        _ ->
            accum

-- [1, 2, 3]
goodOutput =
    makeLazy [ 1, 2, 3 ] [] |> List.map (\f -> f ())

-- [3, 3, 3]
badOutput =
    tcoMakeLazy [ 1, 2, 3 ] [] |> List.map (\f -> f ())


tcoProducesBadClosuresTest = test "Running a TCO version with closures capturing local variables should equal running a TCO version without closures" <|
    \_ -> badOutput |> Expect.equal goodOutput

