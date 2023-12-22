module TCOMiscompilation0 exposing (tcoMiscompilation0Test)

-- From https://github.com/elm/compiler/issues/2221#issue-972780518

import Test exposing (Test, test)
import Expect

g : Int -> (Int -> a) -> a
g value cont =
    case value of
        1 -> cont 1
        _ -> g (value-1) (\result -> cont (result * value))

tcoMiscompilation0Test = test "TCO shouldn't result in a stack overflow for CPS" <|
    \_ -> g 2 identity |> Expect.equal (g 2 identity)
