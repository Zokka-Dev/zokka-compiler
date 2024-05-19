module Page.Search.Entry exposing
  ( Package
  , search
  , dashboardDataDecoder
  , DataFromBackend
  , Repository
  , AuthToken
  , authTokenDecoder
  , backendDataToRepositories
  , newlyCreatedAuthTokenDecoder
  )


import Dict exposing (Dict)
import Elm.Version as V
import Json.Decode as D
import Utils.AuthTokenId exposing (AuthTokenId(..))
import Utils.DictUtils exposing (groupBy)
import Utils.NonEmptyList as NonEmptyList
import Utils.PermissionLevel as PermissionLevel exposing (PermissionLevel)
import Utils.Popularity as Popularity
import Utils.RepositoryId as RepositoryId exposing (RepositoryId(..))



type alias DataFromBackend =
    { authTokens : List AuthToken
    , packages : List Package
    , repoIds : List RepositoryId
    }


type alias Repository =
    { packages : List Package
    , authTokens : List AuthToken
    , id : RepositoryId
    , newTokenValue : Maybe String
    }


type Either a b = Left a | Right b

pullOutRepository : Either Package AuthToken -> String
pullOutRepository entryOrAuthToken = case entryOrAuthToken of
    Left entry -> entry.repositoryId
    Right authToken -> RepositoryId.toString authToken.repository

addEither : Either a b -> (List a, List b) -> (List a, List b)
addEither either (accLeft, accRight) = case either of
    Left a -> (a :: accLeft, accRight)
    Right b -> (accLeft, b :: accRight)

unzipEithers : List (Either a b) -> (List a, List b)
unzipEithers eithers = List.foldr addEither ([], []) eithers

backendDataToRepositories : DataFromBackend -> Dict String Repository
backendDataToRepositories backendData =
    (List.map Left backendData.packages) ++ (List.map Right backendData.authTokens)
        |> groupBy pullOutRepository
        |> Dict.map (\_ v -> NonEmptyList.toList v)
        |> \d -> Dict.union d (Dict.fromList (List.map (\id -> (RepositoryId.toString id, [])) backendData.repoIds))
        |> Dict.map (\_ v -> unzipEithers v)
        |> Dict.map (\id (packages, authTokens) -> {packages=packages, authTokens=authTokens, id=(RepositoryId id), newTokenValue=Nothing})


repositoriesToBackendData : List Repository -> DataFromBackend
repositoriesToBackendData repositories =
    let
        allPackages = List.concatMap .packages repositories
        allAuthTokens = List.concatMap .authTokens repositories
        allRepoIds = List.map .id repositories
    in
    { packages=allPackages
    , authTokens=allAuthTokens
    , repoIds=allRepoIds
    }


-- ENTRY


type alias Package =
  { name : String
  , author : String
  , project : String
  , summary : String
  , license : String
  , version : V.Version
  , repositoryId : String
  }



-- SEARCH


search : String -> Maybe String -> List Repository -> List Repository
search query maybeAuthor repositories =
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

    asBackendData = repositoriesToBackendData repositories

    entriesThatMatchQuery : List Package
    entriesThatMatchQuery =
      List.filter matches (asBackendData.packages)
        |> List.sortBy (\entry -> negate (Popularity.get entry.author entry.project))

    repositoryToPackages : Dict String (List Package)
    repositoryToPackages = groupBy .repositoryId entriesThatMatchQuery
        |> Dict.map (\_ v -> NonEmptyList.toList v)

    newRepositories = List.map (\r -> { r | packages = Maybe.withDefault r.packages (Dict.get (RepositoryId.toString r.id) repositoryToPackages) }) repositories
  in
  newRepositories



-- DECODER

type alias AuthToken =
    { fragment : String
    , permission : PermissionLevel
    , repository : RepositoryId
    , id : AuthTokenId
    }

dashboardDataDecoder : D.Decoder DataFromBackend
dashboardDataDecoder =
    D.map3 (\packages authTokens repoIds -> {packages=packages, authTokens=authTokens, repoIds=repoIds})
        (D.field "packages" (D.list alternativeEntryDecoder))
        (D.field "auth-tokens" (D.list authTokenDecoder))
        (D.field "repository-ids" (D.list RepositoryId.decoder))

alternativeEntryDecoder : D.Decoder Package
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
        |> D.andThen (\i -> if i == "ReadOnly" then D.succeed PermissionLevel.ReadOnly else if i == "ReadWrite" then D.succeed PermissionLevel.ReadWrite else D.fail ("Got " ++ i ++ " which is not a valid permission level"))


authTokenDecoder : D.Decoder AuthToken
authTokenDecoder =
    D.map4 (\fragment permission repository id -> { fragment=fragment, permission=permission, repository=repository, id=id})
        (D.field "fragment" D.string)
        (D.field "permission" permissionLevelDecoder)
        (D.field "repository" (D.map (RepositoryId << String.fromInt) D.int))
        (D.field "id" (D.map (AuthTokenId << String.fromInt) D.int))

newlyCreatedAuthTokenValueDecoder : D.Decoder String
newlyCreatedAuthTokenValueDecoder = D.field "value" D.string

newlyCreatedAuthTokenDecoder : D.Decoder (AuthToken, String)
newlyCreatedAuthTokenDecoder = authTokenDecoder
    |> D.andThen (\authToken -> D.map (\value -> (authToken, value)) newlyCreatedAuthTokenValueDecoder)