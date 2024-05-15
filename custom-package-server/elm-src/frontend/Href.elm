module Href exposing
  ( toAuthor
  , toProject
  , toVersion
  , toAbout
  , toModule
  , toModuleWithSource
  , toModuleWithQuery
  )


import Elm.Version as V
import Url.Builder as Url



-- HREFS


toAuthor : String -> String
toAuthor author =
  Url.absolute [ "packages", author, "" ] []


toProject : String -> String -> String
toProject author project =
  Url.absolute [ "packages", author, project, "" ] []


toVersion : String -> String -> Maybe V.Version -> Maybe String -> String
toVersion author project version maybeValue =
  Url.custom Url.Absolute [ "packages", author, project, vsnToString version, ""] [] maybeValue


toAbout : String -> String -> Maybe V.Version -> String
toAbout author project version =
  Url.absolute [ "packages", author, project, vsnToString version, "about"] []


toModule : String -> String -> Maybe V.Version -> String -> Maybe String -> String
toModule author project version moduleName maybeValue =
  Url.custom Url.Absolute
    [ "packages", author, project, vsnToString version, String.replace "." "-" moduleName ] [] maybeValue


toModuleWithQuery : String -> String -> Maybe V.Version -> String -> Maybe String -> String -> String
toModuleWithQuery author project version moduleName maybeValue query =
  Url.custom Url.Absolute
    [ "packages"
    , author
    , project
    , vsnToString version
    , String.replace "." "-" moduleName
    ]
    (if String.isEmpty query then [] else [ Url.string "q" query ])
    maybeValue


toModuleWithSource : String -> String -> Maybe V.Version -> String -> Maybe String -> String
toModuleWithSource author project version moduleName maybeValue =
  Url.custom Url.Absolute
    [ "packages"
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
