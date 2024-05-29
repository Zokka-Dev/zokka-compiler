module Utils.RepositoryUrlSafeName exposing (..)

type RepositoryUrlSafeName = RepositoryUrlSafeName String

toString : RepositoryUrlSafeName -> String
toString (RepositoryUrlSafeName str) = str