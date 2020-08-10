module Api exposing (Token, authenticate)

import Http
import Process
import Task


type Token
    = Token String


authenticate : Bool -> String -> String -> (Result Http.Error Token -> msg) -> Cmd msg
authenticate demo login password toMsg =
    (if demo then
        Process.sleep 1500
            |> Task.perform (\_ -> Ok (Token "demoTocken"))

     else
        Http.get
            { url = "https://elm-lang.org/assets/public-opinion.txt"
            , expect = Http.expectString (\result -> Result.map Token result)
            }
    )
        |> Cmd.map toMsg
