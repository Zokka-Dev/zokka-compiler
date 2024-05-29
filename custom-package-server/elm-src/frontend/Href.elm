module Href exposing
  ( toAuthor
  , toProject
  , toVersion
  , toAbout
  , toModule
  , toModuleInStandardElmPackageRepo
  , toModuleWithSource
  , toModuleWithQuery
  )


import Elm.Version as V
import Url.Builder as Url



-- HREFS


toAuthor : String -> String -> String
toAuthor repository author =
  Url.absolute [ "repository", repository, "packages", author, "" ] []


toProject : String -> String -> String -> String
toProject repository author project =
  Url.absolute [ "repository", repository, "packages", author, project, "" ] []


toVersion : String -> String -> String -> Maybe V.Version -> Maybe String -> String
toVersion repository author project version maybeValue =
  Url.custom Url.Absolute [ "repository", repository, "packages", author, project, vsnToString version, ""] [] maybeValue


toAbout : String -> String -> String -> Maybe V.Version -> String
toAbout repository author project version =
  Url.absolute [ "repository", repository, "packages", author, project, vsnToString version, "about"] []


toModule : String -> String -> String -> Maybe V.Version -> String -> Maybe String -> String
toModule repository author project version moduleName maybeValue =
  Url.custom Url.Absolute
    [ "repository", repository, "packages", author, project, vsnToString version, String.replace "." "-" moduleName ] [] maybeValue


toModuleInStandardElmPackageRepo : String -> String -> Maybe V.Version -> String -> Maybe String -> String
toModuleInStandardElmPackageRepo author project version moduleName maybeValue =
  Url.custom Url.Absolute
    [ "https://package.elm-lang.org", "packages", author, project, vsnToString version, String.replace "." "-" moduleName ] [] maybeValue


toModuleWithQuery : String -> String -> String -> Maybe V.Version -> String -> Maybe String -> String -> String
toModuleWithQuery repository author project version moduleName maybeValue query =
  Url.custom Url.Absolute
    [ "repository"
    , repository
    , "packages"
    , author
    , project
    , vsnToString version
    , String.replace "." "-" moduleName
    ]
    (if String.isEmpty query then [] else [ Url.string "q" query ])
    maybeValue


toModuleWithSource : String -> String -> String -> Maybe V.Version -> String -> Maybe String -> String
toModuleWithSource repository author project version moduleName maybeValue =
  Url.custom Url.Absolute
    [ "repository"
    , repository
    , "packages"
    , author
    , project
    , vsnToString version
    , String.replace "." "-" moduleName
    ]
    [ Url.string "source" "" ]
    maybeValue



-- HELPERS


vsnToString : Maybe V.Version -> String
vsnToString maybeVersion =
  case maybeVersion of
    Nothing ->
      "latest"

    Just version ->
      V.toString version
