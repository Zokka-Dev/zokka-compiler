module BadOccursCheck2 exposing (..)

-- From https://github.com/elm/compiler/issues/2241#issuecomment-1882268780


type X r
    = X


f : X r -> X { r | y : () }
f _ =
    X


x : X { z : () }
x =
    X


example : ()
example =
    x
        |> (if True then
                f

            else
                identity
           )
        |> always ()
