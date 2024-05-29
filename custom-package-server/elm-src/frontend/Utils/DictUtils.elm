module Utils.DictUtils exposing (..)

import Dict exposing (Dict)
import Utils.NonEmptyList as NonEmptyList exposing (NonEmptyList)


groupBy : (a -> comparable) -> List a -> Dict comparable (NonEmptyList a)
groupBy keyfn list =
    List.foldr
        (\x acc ->
            Dict.update (keyfn x) (Maybe.map (NonEmptyList.cons x) >> Maybe.withDefault (NonEmptyList.singleton x) >> Just) acc
        )
        Dict.empty
        list