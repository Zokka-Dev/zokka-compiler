module Deps.Website
  ( standardElmPkgRepoDomain
  , route
  , metadata
  )
  where


import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Http
import Elm.CustomRepositoryData (RepositoryUrl)
import qualified Data.Utf8 as Utf8



standardElmPkgRepoDomain :: RepositoryUrl
standardElmPkgRepoDomain =
  Utf8.fromChars "https://package.elm-lang.org"


route :: RepositoryUrl -> String -> [(String,String)] -> String
route repositoryUrl path params =
  Http.toUrl (Utf8.toChars repositoryUrl ++ path) params


metadata :: RepositoryUrl -> Pkg.Name -> V.Version -> String -> String
metadata repositoryUrl name version file =
  Utf8.toChars repositoryUrl ++ "/packages/" ++ Pkg.toUrl name ++ "/" ++ V.toChars version ++ "/" ++ file
