module Session exposing
  ( Data
  , empty
  , addEntries
  , getReleases
  , getLatestVersion
  , addReleases
  , fetchReleases
  , getReadme
  , addReadme
  , fetchReadme
  , getDocs
  , addDocs
  , fetchDocs
  , getManifest
  , addManifest
  , fetchManifest
  )


import Dict
import Elm.Docs as Docs
import Elm.Project as Project
import Elm.Version as V
import Http
import Json.Decode as Decode
import Page.Search.Entry as Entry exposing (DataFromBackend)
import Release
import Url.Builder as Url
import Utils.OneOrMore exposing (OneOrMore(..))



-- SESSION DATA


type alias Data =
  { entries : Maybe DataFromBackend
  , releases : Dict.Dict String (OneOrMore Release.Release)
  , readmes : Dict.Dict String String
  , docs : Dict.Dict String (List Docs.Module)
  , manifests: Dict.Dict String Project.PackageInfo
  }


empty : Data
empty =
  { entries = Nothing
  , releases = Dict.empty
  , readmes = Dict.empty
  , docs = Dict.empty
  , manifests = Dict.empty
  }



-- ENTRIES


addEntries : DataFromBackend -> Data -> Data
addEntries entries data =
  { data | entries = Just entries }



-- RELEASES


toPkgKey : String -> String -> String
toPkgKey author project =
  author ++ "/" ++ project


getReleases : Data -> String -> String -> Maybe (OneOrMore Release.Release)
getReleases data author project =
  Dict.get (toPkgKey author project) data.releases


getLatestVersion : Data -> String -> String -> Maybe V.Version
getLatestVersion data author project =
  Maybe.map Release.getLatestVersion (getReleases data author project)

addReleases : String -> String -> OneOrMore Release.Release -> Data -> Data
addReleases author project releases data =
  let
    newReleases =
      Dict.insert (toPkgKey author project) releases data.releases
  in
  { data | releases = newReleases }


fetchReleases : String -> String -> String -> Http.Request (OneOrMore Release.Release)
fetchReleases repository author project =
  Http.get
    (Url.absolute [ "dashboard", "repository", repository, "packages", author, project, "releases.json" ] [])
    Release.decoder



-- README


toVsnKey : String -> String -> V.Version -> String
toVsnKey author project version =
  author ++ "/" ++ project ++ "@" ++ V.toString version


getReadme : Data -> String -> String -> V.Version -> Maybe String
getReadme data author project version =
  Dict.get (toVsnKey author project version) data.readmes


addReadme : String -> String -> V.Version -> String -> Data -> Data
addReadme author project version readme data =
  let
    newReadmes =
      Dict.insert (toVsnKey author project version) readme data.readmes
  in
  { data | readmes = newReadmes }


fetchReadme : String -> String -> String -> V.Version -> Http.Request String
fetchReadme repository author project version =
  Http.getString <|
    Url.absolute [ "dashboard", "repository", repository, "packages", author, project, V.toString version, "README.md" ] []



-- DOCS


getDocs : Data -> String -> String -> V.Version -> Maybe (List Docs.Module)
getDocs data author project version =
  Dict.get (toVsnKey author project version) data.docs


addDocs : String -> String -> V.Version -> List Docs.Module -> Data -> Data
addDocs author project version docs data =
  let
    newDocs =
      Dict.insert (toVsnKey author project version) docs data.docs
  in
  { data | docs = newDocs }


fetchDocs : String -> String -> String -> V.Version -> Http.Request (List Docs.Module)
fetchDocs repository author project version =
  Http.get
    (Url.absolute [ "dashboard", "repository", repository, "packages", author, project, V.toString version, "docs.json" ] [])
    (Decode.list Docs.decoder)



-- ELM.JSON MANIFESTS


getManifest : Data -> String -> String -> V.Version -> Maybe Project.PackageInfo
getManifest data author project version =
    Dict.get (toVsnKey author project version) data.manifests


addManifest : String -> String -> V.Version -> Project.PackageInfo -> Data -> Data
addManifest author project version manifest data =
  let
    newManifests =
      Dict.insert (toVsnKey author project version) manifest data.manifests
  in
  { data | manifests = newManifests }


fetchManifest : String -> String -> String -> V.Version -> Http.Request Project.PackageInfo
fetchManifest repository author project version =
  Http.get
    (Url.absolute [ "dashboard", "repository", repository, "packages", author, project, V.toString version, "elm.json" ] [])
    packageInfoDecoder


packageInfoDecoder : Decode.Decoder Project.PackageInfo
packageInfoDecoder =
  Decode.andThen
    (\project ->
      case project of
        Project.Application _ -> Decode.fail "Unexpected application"
        Project.Package info  -> Decode.succeed info
    )
  Project.decoder
