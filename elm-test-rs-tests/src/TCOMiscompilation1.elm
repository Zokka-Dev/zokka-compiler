module TCOMiscompilation1 exposing (tcoMiscompilation1Test)

-- https://github.com/elm/compiler/issues/2268#issuecomment-1671931317

import Test exposing (Test, test)
import Expect

type Trampoline a
    = More (() -> Trampoline a)
    | Done a


wrapMany : Int -> Trampoline a -> Trampoline a
wrapMany n trampoline =
    if n > 0 then
        wrapMany (n - 1) (More (\_ -> trampoline))

    else
        trampoline


run : Trampoline a -> a
run trampoline =
    case trampoline of
        More next ->
            run (next ())

        Done a ->
            a

wrapManyNoBug : Int -> Trampoline a -> Trampoline a
wrapManyNoBug n trampoline =
    if n > 0 then
        wrapManyNoBug (n - 1) (more trampoline)

    else
        trampoline


more : Trampoline a -> Trampoline a
more a =
    More (\_ -> a)

trampolinedValue = More(\_ -> (More (\_ -> Done 0)))

-- This should cause hanging in vanilla Elm
buggyExample = run (wrapMany 2 trampolinedValue)


-- This shouldn't hang in vanilla Elm
goodExample = run (wrapManyNoBug 2 trampolinedValue)


tcoMiscompilation1Test = test "TCO shouldn't result in a stack overflow for trampolines" <|
    \_ ->  buggyExample |> Expect.equal goodExample
