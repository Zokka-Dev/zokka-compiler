module Utils.RepositoryName exposing (..)

type RepositoryName = RepositoryName String

toString : RepositoryName -> String
toString (RepositoryName str) = str