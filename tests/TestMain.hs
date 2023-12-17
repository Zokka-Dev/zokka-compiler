{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty.Hedgehog as HH

import qualified Data.Set as Set

import Data.Function ((&))
import Hedgehog ((===))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Deps.Registry (ZokkaRegistries (..), RegistryKey(..), Registry(..), KnownVersions(..))
import qualified Data.Binary as Binary
import qualified Data.Utf8
import qualified Data.Utf8 as Utf8
import qualified Data.Map.Strict as Map
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import File (Time(..))
import Data.Fixed (Fixed(..))

utf8String :: Hedgehog.Gen (Utf8.Utf8 a)
utf8String = Utf8.fromChars <$> Gen.string (Range.linear 0 40) Gen.unicode

registryKeyGen :: Hedgehog.Gen RegistryKey
registryKeyGen = Gen.choice [ fmap PackageUrlKey utf8String, fmap RepositoryUrlKey utf8String ]

knownVersionsGen :: Hedgehog.Gen KnownVersions
knownVersionsGen = do
  version <- versionGen
  previousVersions <- Gen.list (Range.linear 0 10) versionGen
  pure KnownVersions{_newest=version, _previous=previousVersions}

registryGen :: Hedgehog.Gen Registry
registryGen = do
    versionsMap <- Gen.map (Range.linear 0 10) ((,) <$> pkgNameGen <*> knownVersionsGen)
    pure Registry{_count=Map.size versionsMap, _versions=versionsMap}

registryKeyToRegistryGen :: Hedgehog.Gen (Map.Map RegistryKey Registry)
registryKeyToRegistryGen = Gen.map (Range.linear 0 10) ((,) <$> registryKeyGen <*> registryGen)

versionToRegistryKeyGen :: Hedgehog.Gen (Map.Map V.Version RegistryKey)
versionToRegistryKeyGen = Gen.map (Range.linear 0 10) ((,) <$> versionGen <*> registryKeyGen)

versionGen :: Hedgehog.Gen V.Version
versionGen = do
  major <- Gen.word16 (Range.linear 0 10)
  minor <- Gen.word16 (Range.linear 0 10)
  patch <- Gen.word16 (Range.linear 0 10)
  pure V.Version{V._major=major, V._minor=minor, V._patch=patch}

authorGen :: Hedgehog.Gen Pkg.Author
authorGen = utf8String

projectGen :: Hedgehog.Gen Pkg.Project
projectGen = utf8String

pkgNameGen :: Hedgehog.Gen Pkg.Name
pkgNameGen = do
  author <- authorGen
  project <- projectGen
  pure Pkg.Name{Pkg._author=author, Pkg._project=project}

packagesToLocationsGen :: Hedgehog.Gen (Map.Map Pkg.Name (Map.Map V.Version RegistryKey))
packagesToLocationsGen = Gen.map (Range.linear 0 10) ((,) <$> pkgNameGen <*> versionToRegistryKeyGen)

fixedGen :: Hedgehog.Gen (Fixed a)
fixedGen = MkFixed <$> Gen.integral (Range.linear (-1_000_000_000_000_000) 1_000_000_000_000_000)

timeGen :: Hedgehog.Gen Time
timeGen = Time <$> fixedGen

zokkaRegistriesGen :: Hedgehog.Gen ZokkaRegistries
zokkaRegistriesGen = do
  registryKeyToRegistry <- registryKeyToRegistryGen
  packagesToLocations <- packagesToLocationsGen
  time <- timeGen
  pure (ZokkaRegistries{_lastModificationTimeOfCustomRepoConfig=time, _registries=registryKeyToRegistry, _packagesToLocations=packagesToLocations})

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ properties
  , unitTests
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ hedgehogProperties
  ]

hedgehogProperties :: TestTree
hedgehogProperties = testGroup "(checked by Hedgehog)"
  [ HH.testProperty "dummy property" $
      dummyProperty
  , HH.testProperty "make sure roundtrip works" $
      roundtripBinaryEncodingOfZokkaRegistryChangesNothing
  ]

dummyProperty :: Hedgehog.Property
dummyProperty =
    Hedgehog.property $ do
        x <- Hedgehog.forAll $ Gen.int (Range.linear 1 10)
        x === x

roundtripBinaryEncodingOfZokkaRegistryChangesNothing :: Hedgehog.Property
roundtripBinaryEncodingOfZokkaRegistryChangesNothing =
    Hedgehog.property $ do
        x <- Hedgehog.forAll zokkaRegistriesGen
        Binary.decode (Binary.encode x) === x

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "dummy unit test" $
      1 @?= 1
  ]
