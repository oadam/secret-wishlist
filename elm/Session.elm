module Session exposing (Session)

import Api exposing (Token, User)


type alias Session =
    { token : Token
    , users : List User
    , logged_user : User
    }
