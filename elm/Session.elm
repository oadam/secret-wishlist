module Session exposing (Session)

import Api exposing (Token)


type alias Session =
    { token : Token
    , user : String
    }
