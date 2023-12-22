module TCOMiscompilation2 exposing (tcoMiscompilation2Test)

-- From https://github.com/elm/compiler/issues/1813#issue-365193260

import Test exposing (Test, test)
import Expect

testcase = let loop n list = if n <= 0 then list else loop (n - 1) ((\() -> n) :: list) in List.map (\f -> f()) <| loop 3 []

tcoMiscompilation2Test = test "TCO not just repeat the same value over and over again when given a trivial loop" <|
    \_ -> testcase |> Expect.equal [1, 2, 3]
