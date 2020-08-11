module Api exposing (Token, authenticate)

import Http
import Process
import Task exposing (Task)


type alias Token =
    { demo : Bool, token : String }


mockHttpGet : a -> Cmd (Result x a)
mockHttpGet result =
    Process.sleep 1500
        |> Task.andThen (\_ -> Task.succeed result)
        |> Task.perform Ok


authenticate : ( String, String ) -> (Result Http.Error Token -> msg) -> Cmd msg
authenticate ( login, password ) toMsg =
    let
        demo =
            login == "demo"

        stringToToken =
            -- partial application of Token
            Token demo

        tokenStringCommand =
            if demo then
                mockHttpGet "mockToken"

            else
                Http.get
                    { url = "/api/login/"
                    , expect = Http.expectString identity
                    }
    in
    tokenStringCommand
        |> Cmd.map (Result.map stringToToken)
        |> Cmd.map toMsg
