{-# LANGUAGE OverloadedStrings #-}
module Deps.Diff
  ( diff
  , PackageChanges(..)
  , ModuleChanges(..)
  , Changes(..)
  , moduleChangeMagnitude
  , toMagnitude
  , bump
  , getDocs
  )
  where


import Control.Monad (zipWithM)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Deps.Website as Website
import qualified Elm.Compiler.Type as Type
import qualified Elm.Docs as Docs
import qualified Elm.Magnitude as M
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Http
import qualified Json.Decode as D
import qualified Reporting.Exit as Exit
import qualified Stuff
import Deps.Registry (ZokkaRegistries, createAuthHeader)
import qualified Deps.Registry as Registry
import qualified Data.Utf8 as Utf8
import Logging.Logger (printLog)
import Elm.CustomRepositoryData (CustomSingleRepositoryData(..), CustomRepositoriesData, DefaultPackageServerRepo(..), PZRPackageServerRepo(..))



-- CHANGES


data PackageChanges =
  PackageChanges
    { _modules_added :: [ModuleName.Raw]
    , _modules_changed :: Map.Map ModuleName.Raw ModuleChanges
    , _modules_removed :: [ModuleName.Raw]
    }


data ModuleChanges =
  ModuleChanges
    { _unions :: Changes Name.Name Docs.Union
    , _aliases :: Changes Name.Name Docs.Alias
    , _values :: Changes Name.Name Docs.Value
    , _binops :: Changes Name.Name Docs.Binop
    }


data Changes k v =
  Changes
    { _added :: Map.Map k v
    , _changed :: Map.Map k (v,v)
    , _removed :: Map.Map k v
    }


getChanges :: (Ord k) => (v -> v -> Bool) -> Map.Map k v -> Map.Map k v -> Changes k v
getChanges isEquivalent old new =
  let
    overlap = Map.intersectionWith (,) old new
    changed = Map.filter (not . uncurry isEquivalent) overlap
  in
    Changes (Map.difference new old) changed (Map.difference old new)



-- DIFF


diff :: Docs.Documentation -> Docs.Documentation -> PackageChanges
diff oldDocs newDocs =
  let
    filterOutPatches chngs =
      Map.filter (\chng -> moduleChangeMagnitude chng /= M.PATCH) chngs

    (Changes added changed removed) =
      getChanges (\_ _ -> False) oldDocs newDocs
  in
    PackageChanges
      (Map.keys added)
      (filterOutPatches (Map.map diffModule changed))
      (Map.keys removed)



diffModule :: (Docs.Module, Docs.Module) -> ModuleChanges
diffModule (Docs.Module _ _ u1 a1 v1 b1, Docs.Module _ _ u2 a2 v2 b2) =
  ModuleChanges
    (getChanges isEquivalentUnion u1 u2)
    (getChanges isEquivalentAlias a1 a2)
    (getChanges isEquivalentValue v1 v2)
    (getChanges isEquivalentBinop b1 b2)



-- EQUIVALENCE


isEquivalentUnion :: Docs.Union -> Docs.Union -> Bool
isEquivalentUnion (Docs.Union oldComment oldVars oldCtors) (Docs.Union newComment newVars newCtors) =
    length oldCtors == length newCtors
    && and (zipWith (==) (map fst oldCtors) (map fst newCtors))
    && and (Map.elems (Map.intersectionWith equiv (Map.fromList oldCtors) (Map.fromList newCtors)))
  where
    equiv :: [Type.Type] -> [Type.Type] -> Bool
    equiv oldTypes newTypes =
      let
        allEquivalent =
          zipWith
            isEquivalentAlias
            (map (Docs.Alias oldComment oldVars) oldTypes)
            (map (Docs.Alias newComment newVars) newTypes)
      in
        length oldTypes == length newTypes
        && and allEquivalent


isEquivalentAlias :: Docs.Alias -> Docs.Alias -> Bool
isEquivalentAlias (Docs.Alias _ oldVars oldType) (Docs.Alias _ newVars newType) =
  case diffType oldType newType of
    Nothing ->
      False

    Just renamings ->
      length oldVars == length newVars
      && isEquivalentRenaming (zip oldVars newVars ++ renamings)


isEquivalentValue :: Docs.Value -> Docs.Value -> Bool
isEquivalentValue (Docs.Value c1 t1) (Docs.Value c2 t2) =
  isEquivalentAlias (Docs.Alias c1 [] t1) (Docs.Alias c2 [] t2)


isEquivalentBinop :: Docs.Binop -> Docs.Binop -> Bool
isEquivalentBinop (Docs.Binop c1 t1 a1 p1) (Docs.Binop c2 t2 a2 p2) =
  isEquivalentAlias (Docs.Alias c1 [] t1) (Docs.Alias c2 [] t2)
  && a1 == a2
  && p1 == p2



-- DIFF TYPES


diffType :: Type.Type -> Type.Type -> Maybe [(Name.Name,Name.Name)]
diffType oldType newType =
  case (oldType, newType) of
    (Type.Var oldName, Type.Var newName) ->
      Just [(oldName, newName)]

    (Type.Lambda a b, Type.Lambda a' b') ->
      (++)
        <$> diffType a a'
        <*> diffType b b'

    (Type.Type oldName oldArgs, Type.Type newName newArgs) ->
      if not (isSameName oldName newName) || length oldArgs /= length newArgs then
        Nothing
      else
        concat <$> zipWithM diffType oldArgs newArgs

    (Type.Record fields maybeExt, Type.Record fields' maybeExt') ->
      case (maybeExt, maybeExt') of
        (Nothing, Just _) ->
          Nothing

        (Just _, Nothing) ->
          Nothing

        (Nothing, Nothing) ->
          diffFields fields fields'

        (Just oldExt, Just newExt) ->
          (:) (oldExt, newExt) <$> diffFields fields fields'

    (Type.Unit, Type.Unit) ->
      Just []

    (Type.Tuple a b cs, Type.Tuple x y zs) ->
      if length cs /= length zs then
        Nothing
      else
        do  aVars <- diffType a x
            bVars <- diffType b y
            cVars <- concat <$> zipWithM diffType cs zs
            return (aVars ++ bVars ++ cVars)

    (_, _) ->
      Nothing


-- handle very old docs that do not use qualified names
isSameName :: Name.Name -> Name.Name -> Bool
isSameName oldFullName newFullName =
  let
    dedot name =
      reverse (Name.splitDots name)
  in
    case ( dedot oldFullName, dedot newFullName ) of
      (oldName:[], newName:_) ->
        oldName == newName

      (oldName:_, newName:[]) ->
        oldName == newName

      _ ->
        oldFullName == newFullName


diffFields :: [(Name.Name, Type.Type)] -> [(Name.Name, Type.Type)] -> Maybe [(Name.Name,Name.Name)]
diffFields oldRawFields newRawFields =
  let
    sort = List.sortBy (compare `on` fst)
    oldFields = sort oldRawFields
    newFields = sort newRawFields
  in
    if length oldRawFields /= length newRawFields then
      Nothing

    else if or (zipWith ((/=) `on` fst) oldFields newFields) then
      Nothing

    else
      concat <$> zipWithM (diffType `on` snd) oldFields newFields



-- TYPE VARIABLES


isEquivalentRenaming :: [(Name.Name,Name.Name)] -> Bool
isEquivalentRenaming varPairs =
  let
    renamings =
      Map.toList (foldr insert Map.empty varPairs)

    insert (old,new) dict =
      Map.insertWith (++) old [new] dict

    verify (old, news) =
      case news of
        [] ->
          Nothing

        new : rest ->
          if all (new ==) rest then
            Just (old, new)
          else
            Nothing

    allUnique list =
      length list == Set.size (Set.fromList list)
  in
    case mapM verify renamings of
      Nothing ->
        False

      Just verifiedRenamings ->
        all compatibleVars verifiedRenamings
        &&
        allUnique (map snd verifiedRenamings)


compatibleVars :: (Name.Name, Name.Name) -> Bool
compatibleVars (old, new) =
  case (categorizeVar old, categorizeVar new) of
    (CompAppend, CompAppend) -> True
    (Comparable, Comparable) -> True
    (Appendable, Appendable) -> True
    (Number    , Number    ) -> True
    (Number    , Comparable) -> True

    (_, Var) -> True

    (_, _) -> False


data TypeVarCategory
  = CompAppend
  | Comparable
  | Appendable
  | Number
  | Var


categorizeVar :: Name.Name -> TypeVarCategory
categorizeVar name
  | Name.isCompappendType name = CompAppend
  | Name.isComparableType name = Comparable
  | Name.isAppendableType name = Appendable
  | Name.isNumberType     name = Number
  | otherwise                  = Var



-- MAGNITUDE


bump :: PackageChanges -> V.Version -> V.Version
bump changes version =
  case toMagnitude changes of
    M.PATCH ->
      V.bumpPatch version

    M.MINOR ->
      V.bumpMinor version

    M.MAJOR ->
      V.bumpMajor version


toMagnitude :: PackageChanges -> M.Magnitude
toMagnitude (PackageChanges added changed removed) =
  let
    addMag = if null added then M.PATCH else M.MINOR
    removeMag = if null removed then M.PATCH else M.MAJOR
    changeMags = map moduleChangeMagnitude (Map.elems changed)
  in
    maximum (addMag : removeMag : changeMags)


moduleChangeMagnitude :: ModuleChanges -> M.Magnitude
moduleChangeMagnitude (ModuleChanges unions aliases values binops) =
  maximum
    [ changeMagnitude unions
    , changeMagnitude aliases
    , changeMagnitude values
    , changeMagnitude binops
    ]


changeMagnitude :: Changes k v -> M.Magnitude
changeMagnitude (Changes added changed removed) =
  if Map.size removed > 0 || Map.size changed > 0 then
    M.MAJOR

  else if Map.size added > 0 then
    M.MINOR

  else
    M.PATCH



-- GET DOCS


getDocsFromCustomSingleRepositoryData :: CustomRepositoriesData -> IO ()
getDocsFromCustomSingleRepositoryData customRepositoriesData =
  do
    pure ()


getDocs :: Stuff.PackageCache -> ZokkaRegistries -> Http.Manager -> Pkg.Name -> V.Version -> IO (Either Exit.DocsProblem Docs.Documentation)
getDocs cache zokkaRegistry manager name version =
  do  let home = Stuff.package cache name version
      let path = home </> "docs.json"
      exists <- File.exists path
      if exists
        then
          do  bytes <- File.readUtf8 path
              case D.fromByteString Docs.decoder bytes of
                Right docs ->
                  return $ Right docs

                Left _ ->
                  do  File.remove path
                      return $ Left Exit.DP_Cache
        else
          do  let registryKeyMaybe = Registry.lookupPackageRegistryKey zokkaRegistry name version
              -- FIXME: Handle the non-repository URL case better
              repositoryData <- case registryKeyMaybe of
                Just (Registry.RepositoryUrlKey repositoryUrl) -> pure repositoryUrl
                _ -> printLog "Had a bad thing happen in getDocs" >> undefined -- FIXME: Should really get rid of this!
              -- FIXME: Try to dedupe some of this
              case repositoryData of
                DefaultPackageServerRepoData defaultPackageServerRepo ->
                  do
                    let repositoryUrl = _defaultPackageServerRepoTypeUrl defaultPackageServerRepo
                    let url = Website.metadata repositoryUrl name version "docs.json"
                    Http.get manager url [] Exit.DP_Http $ \body ->
                      case D.fromByteString Docs.decoder body of
                        Right docs ->
                          do  Dir.createDirectoryIfMissing True home
                              File.writeUtf8 path body
                              return $ Right docs

                        Left _ ->
                          return $ Left $ Exit.DP_Data url body
                PZRPackageServerRepoData pzrPackageServerRepo ->
                  do
                    let repositoryUrl = _pzrPackageServerRepoTypeUrl pzrPackageServerRepo
                    let repoAuthToken = _pzrPackageServerRepoAuthToken pzrPackageServerRepo
                    let url = Website.metadata repositoryUrl name version "docs.json"
                    Http.get manager url [createAuthHeader repoAuthToken] Exit.DP_Http $ \body ->
                      case D.fromByteString Docs.decoder body of
                        Right docs ->
                          do  Dir.createDirectoryIfMissing True home
                              File.writeUtf8 path body
                              return $ Right docs

                        Left _ ->
                          return $ Left $ Exit.DP_Data url body