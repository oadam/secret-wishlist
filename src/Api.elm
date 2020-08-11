module Api exposing (Token, authenticate)

import Http
import Process
import Task exposing (Task)


type alias Token =
    { demo : Bool, token : String }


mockHttpGet : a -> (Result Http.Error a -> msg) -> Cmd msg
mockHttpGet result toMsg =
    Process.sleep 1500
        |> Task.andThen (\_ -> Task.succeed (Ok result))
        |> Task.perform toMsg


authenticate : ( String, String ) -> (Result Http.Error Token -> msg) -> Cmd msg
authenticate ( login, password ) toMsg =
    let
        demo =
            login == "demo"

        -- partial application of Token
        stringToToken =
            Token demo
    in
    if demo then
        mockHttpGet (Token True "mockToken") toMsg

    else
        Http.get
            { url = "/api/login/"
            , expect = Http.expectString (Result.map stringToToken >> toMsg)
            }
