module AnotherBadClosure exposing (anotherBadClosureTest)

-- From https://github.com/elm/compiler/issues/1813#issuecomment-535417649

import Test exposing (Test, test)
import Expect


{-| This is a tail-recursive function.

It adds upwards counting integers starting from 0 to the given list:

    fDirect 4 [ 0 ] == [ 0, 1, 2, 3, 4, 0 ]

-}
fDirect : Int -> List Int -> List Int
fDirect count accum =
    if count < 0 then
        accum

    else
        fDirect (count - 1) (count :: accum)


{-| This is a slight variation of f and also tail-recursive.

If we add a closure around the reference of "count", one of
the arguments that tail-call optimization transforms into a variable,
then we get incorrect values.

    List.map ((|>) ()) (f 4 [ \() -> 0 ]) == [ -1, -1, -1, -1, -1, 0 ]

-}
f : Int -> List (() -> Int) -> List (() -> Int)
f count accum =
    if count < 0 then
        accum

    else
        f (count - 1) ((\() -> count) :: accum)


{-| results in [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

We apply `()` on all functions in the generated list, to collapse the closures.

-}
bug : List Int
bug =
    List.map ((|>) ()) (f 10 [])


{-| results in [0,1,2,3,4,5,6,7,8,9,10]
-}
nobug : List Int
nobug =
    fDirect 10 []


anotherBadClosureTest = test "Running a TCO version with closures that capture function arguments should equal running a TCO version without closures" <|
    \_ -> bug |> Expect.equal nobug
