module Utils.HttpHelpers exposing (..)

import Http

httpPostIgnoreResponseBody : String -> Http.Body -> c -> Http.Request c
httpPostIgnoreResponseBody url body valueToReturnOnSuccess = Http.request
  { method = "POST"
  , headers = []
  , url = url
  , body = body
  -- We don't actually care about the response value at all
  , expect = Http.expectStringResponse (\_ -> Ok valueToReturnOnSuccess)
  , timeout = Nothing
  , withCredentials = False
  }

