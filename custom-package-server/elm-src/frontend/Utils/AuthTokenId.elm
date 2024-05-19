module Utils.AuthTokenId exposing (..)

type AuthTokenId = AuthTokenId String

toString : AuthTokenId -> String
toString (AuthTokenId str) = str
