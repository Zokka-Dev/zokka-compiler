module BadOccursCheck exposing (..)

-- https://github.com/elm/compiler/issues/2241

import Test exposing (test)
import Expect

foldMap : (a -> b) -> (a -> c) -> (b -> c -> c) -> a -> c
foldMap fab fac fbc a = fac a

break : Float -> (Float, Float)
break input = foldMap identity (\x -> (x, x)) (\( low, high ) x -> ( min low x, max high x )) input

badOccursCheckTest = test "Test that this shouldn't cause the compiler to hang (we have a trivial runtime equality check here because this is primarily testing a compile-time bug, not a runtime one)" <|
    \_ -> break 0 |> Expect.equal (break 0)
