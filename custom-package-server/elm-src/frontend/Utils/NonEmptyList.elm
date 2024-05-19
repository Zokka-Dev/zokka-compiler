module Utils.NonEmptyList exposing (..)

type NonEmptyList a =
  NonEmptyList a (List a)


singleton : a -> NonEmptyList a
singleton x = NonEmptyList x []


cons : a -> NonEmptyList a -> NonEmptyList a
cons x (NonEmptyList y ys) = NonEmptyList x (y :: ys)


toList : NonEmptyList a -> List a
toList (NonEmptyList x xs) = x :: xs


head : NonEmptyList a -> a
head (NonEmptyList x _) = x


maxIn : (a -> comparable) -> NonEmptyList a -> a
maxIn f (NonEmptyList x xs) = List.foldl (\acc a -> if f acc > f a then acc else a) x xs