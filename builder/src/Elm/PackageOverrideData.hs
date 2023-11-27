
{-# LANGUAGE EmptyDataDecls #-}
module Elm.PackageOverrideData
  ( PackageOverrideData(..)
  )
  where

import Elm.Version (Version)
import Elm.Package (Name)

data PackageOverrideData = 
  PackageOverrideData
    { _overridePackageName :: !Name
    , _overridePackageVersion :: !Version
    , _originalPackageName :: !Name
    , _originalPackageVersion :: !Version
    }
    deriving Show
