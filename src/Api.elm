module Api exposing (Token, authenticate)

import Http
import Process
import Task exposing (Task)


type Token
    = Token String


type alias Credentials =
    { demo : Bool
    , login : String
    , password : String
    }


mockHttpGet : a -> Cmd (Result x a)
mockHttpGet result =
    Process.sleep 1500
        |> Task.andThen (\_ -> Task.succeed result)
        |> Task.perform Ok


authenticate : Credentials -> (Result Http.Error Token -> msg) -> Cmd msg
authenticate credentials toMsg =
    let
        tokenStringCommand =
            if credentials.demo then
                mockHttpGet "mockToken"

            else
                Http.get
                    { url = "/api/login/"
                    , expect = Http.expectString identity
                    }
    in
    tokenStringCommand
        |> Cmd.map (Result.map Token)
        |> Cmd.map toMsg
