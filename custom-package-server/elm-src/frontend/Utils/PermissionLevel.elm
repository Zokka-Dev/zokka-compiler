module Utils.PermissionLevel exposing (..)

type PermissionLevel = ReadOnly | ReadWrite

toStringInUrlQueryParam : PermissionLevel -> String
toStringInUrlQueryParam permission = case permission of
    ReadOnly -> "readonly"
    ReadWrite -> "readwrite"


