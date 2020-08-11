module Api exposing (Token, authenticate)

import Http
import Process
import Task


type Token
    = Token String


type alias Credentials =
    { demo : Bool
    , login : String
    , password : String
    }


authenticate : Credentials -> (Result Http.Error Token -> msg) -> Cmd msg
authenticate credentials toMsg =
    let
        tokenStringCommand =
            if credentials.demo then
                Process.sleep 1500
                    |> Task.andThen (\_ -> Task.succeed "demoTocken")
                    |> Task.perform Ok

            else
                Http.get
                    { url = "/api/login/"
                    , expect = Http.expectString identity
                    }
    in
    tokenStringCommand
        |> Cmd.map (Result.map Token)
        |> Cmd.map toMsg
