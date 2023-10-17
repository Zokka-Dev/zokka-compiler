{-# LANGUAGE TupleSections #-}
module Data.Map.Utils
  ( fromKeys
  , fromKeysA
  , fromValues
  , any
  , invertMap
  , exchangeKeys
  )
  where


import Prelude hiding (any)
import qualified Data.Map as Map
import Data.Map.Internal (Map(..))
import qualified Data.NonEmptyList as NE
import Control.Monad (join)



-- FROM KEYS


fromKeys :: (Ord k) => (k -> v) -> [k] -> Map.Map k v
fromKeys toValue keys =
  Map.fromList $ map (\k -> (k, toValue k)) keys


fromKeysA :: (Applicative f, Ord k) => (k -> f v) -> [k] -> f (Map.Map k v)
fromKeysA toValue keys =
  Map.fromList <$> traverse (\k -> (,) k <$> toValue k) keys


fromValues :: (Ord k) => (v -> k) -> [v] -> Map.Map k v
fromValues toKey values =
  Map.fromList $ map (\v -> (toKey v, v)) values



-- ANY


{-# INLINE any #-}
any :: (v -> Bool) -> Map.Map k v -> Bool
any isGood = go
  where
    go Tip = False
    go (Bin _ _ v l r) = isGood v || go l || go r

-- FOR TRANSFORMING NESTED MAPS

addToTuple :: a -> (b, c) -> (a, b, c)
addToTuple k (k1, v) = (k, k1, v)

flattenMaps :: Map.Map k0 (Map.Map k1 v) -> [(k0, k1, v)]
flattenMaps nestedMaps =
  --FIXME let's not just have a, b, c as names
  let
    mapAsList = Map.toList nestedMaps
    listOfLists = fmap (\(k, innerMap) -> fmap (addToTuple k) (Map.toList innerMap)) mapAsList
    result = join listOfLists
  in
    result

exchangeKeys :: (Ord k1, Ord k0) => Map.Map k0 (Map.Map k1 v) -> Map.Map k1 (Map.Map k0 v)
exchangeKeys nestedMap = 
  let
    asTriples = flattenMaps nestedMap
    rearrangedTriples = fmap (\(a, b, c) -> (b, [(a, c)])) asTriples
    outerMap = Map.fromListWith (++) rearrangedTriples
  in
    fmap Map.fromList outerMap


invertMap :: (Ord v) => Map.Map k (NE.List v) -> Map.Map v (NE.List k)
invertMap mapOfLists = 
  let 
    mapAsList = Map.toList mapOfLists
    listOfLists = fmap (\(k, vs) -> fmap (k,) vs) mapAsList
    listOfLists' = NE.toList =<< listOfLists
    swappedList = fmap (\(x, y) -> (y, NE.singleton x)) listOfLists'
    result = Map.fromListWith NE.append swappedList
  in 
    result