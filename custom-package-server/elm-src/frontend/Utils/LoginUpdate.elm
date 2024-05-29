module Utils.LoginUpdate exposing (..)

import Http
type LoginUpdate
    = ConfirmedUserIsLoggedIn
    | ConfirmedUserIsNotLoggedIn
    | ConfirmedUserIsLoggedInButWithWrongCreds
    | NoUpdateAboutLoginStatus

httpErrorToLoginUpdate : Http.Error -> LoginUpdate
httpErrorToLoginUpdate httpError =
    case Debug.log "httpError" httpError of
        Http.BadUrl _ -> NoUpdateAboutLoginStatus
        Http.Timeout -> NoUpdateAboutLoginStatus
        Http.NetworkError -> NoUpdateAboutLoginStatus
        Http.BadStatus response -> case response.status.code of
            401 -> ConfirmedUserIsNotLoggedIn
            403 -> ConfirmedUserIsLoggedInButWithWrongCreds
            _ -> NoUpdateAboutLoginStatus
        Http.BadPayload _ _ -> NoUpdateAboutLoginStatus
