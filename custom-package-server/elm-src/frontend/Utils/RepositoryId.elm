module Utils.RepositoryId exposing (..)

import Json.Decode as Decode
type RepositoryId = RepositoryId String

toString : RepositoryId -> String
toString (RepositoryId str) = str

decoder : Decode.Decoder RepositoryId
decoder = Decode.map (\i -> RepositoryId (String.fromInt i)) Decode.int