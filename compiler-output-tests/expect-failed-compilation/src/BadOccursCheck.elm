module BadOccursCheck exposing (..)

-- https://github.com/elm/compiler/issues/2241

foldMap : (a -> b) -> (a -> c) -> (b -> c -> c) -> a -> c
foldMap fab fac fbc a = fac a

f : a -> a -> a
f x y = x

break : Float -> (Float, Float)
break input = foldMap identity (\x -> (x, x)) (\( low, high ) x -> ( f low x, f high x )) input
