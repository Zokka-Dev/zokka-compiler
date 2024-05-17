module Page.Search.Entry exposing
  ( Entry
  , search
  , dashboardDataDecoder
  , redecodeBackToEntries
  )


import Elm.Version as V
import Json.Decode as D
import Utils.Popularity as Popularity



-- ENTRY


type alias Entry =
  { name : String
  , author : String
  , project : String
  , summary : String
  , license : String
  , version : V.Version
  , repositoryId : String
  }



-- SEARCH


search : String -> Maybe String -> List Entry -> List Entry
search query maybeAuthor entries =
  let
    queryTerms =
      String.words (String.toLower query)

    matchesAllTerms entry =
      let
        lowerName =
          String.toLower entry.name

        lowerSummary =
          String.toLower entry.summary

        matchesTerm term =
          String.contains term lowerName
          || String.contains term lowerSummary
      in
      List.all matchesTerm queryTerms

    matchesAuthor entry =
      case maybeAuthor of
        Nothing     -> True
        Just author -> entry.author == author

    matches entry =
      matchesAllTerms entry && matchesAuthor entry
  in
  List.filter matches entries
    |> List.sortBy (\entry -> negate (Popularity.get entry.author entry.project))



-- DECODER

type PermissionLevel = ReadOnly | ReadWrite

type alias AuthToken =
    { fragment : String
    , permission : PermissionLevel
    }

type alias DashboardData =
    { packages : List Entry
    , authTokens : List AuthToken
    }

dashboardDataDecoder : D.Decoder DashboardData
dashboardDataDecoder =
    D.map2 (\packages authTokens -> {packages=packages, authTokens=authTokens})
        (D.field "packages" (D.list alternativeEntryDecoder))
        (D.field "auth-tokens" (D.list authTokenDecoder))

redecodeBackToEntries : D.Decoder (List Entry)
redecodeBackToEntries = D.map .packages dashboardDataDecoder

alternativeEntryDecoder : D.Decoder Entry
alternativeEntryDecoder =
    D.map5
        (\author project version hash repositoryId ->
            { author = author
            , project = project
            , name = String.concat [author, "/", project]
            , summary = ""
            , license = ""
            , version = version
            , repositoryId = String.fromInt repositoryId
            }
        )
        (D.field "author" D.string)
        (D.field "project" D.string)
        (D.field "version" V.decoder)
        (D.field "hash" D.string)
        (D.field "repositoryId" D.int)

permissionLevelDecoder : D.Decoder PermissionLevel
permissionLevelDecoder =
    D.string
        |> D.andThen (\i -> if i == "ReadOnly" then D.succeed ReadOnly else if i == "ReadWrite" then D.succeed ReadWrite else D.fail ("Got " ++ i ++ " which is not a valid permission level"))


authTokenDecoder : D.Decoder AuthToken
authTokenDecoder =
    D.map2 (\fragment permission -> { fragment=fragment, permission=permission})
        (D.field "fragment" D.string)
        (D.field "permission" permissionLevelDecoder)


