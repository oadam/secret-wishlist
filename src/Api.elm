module Api exposing (Token, authenticate)

import Http
import Process
import Task exposing (Task)


type alias Token =
    { demo : Bool, token : String }


mockHttpGet : (Result Http.Error a -> msg) -> a -> Cmd msg
mockHttpGet toMsg result =
    Process.sleep 1500
        |> Task.andThen (\_ -> Task.succeed (Ok result))
        |> Task.perform toMsg


authenticate : ( String, String ) -> (Result Http.Error Token -> msg) -> Cmd msg
authenticate ( login, password ) toMsg =
    if login == "demo" then
        mockHttpGet toMsg <| Token True "mockToken"

    else
        Http.get
            { url = "/api/login/"

            -- `Token False` is of type String -> Token
            , expect =
                Result.map (Token False)
                    >> toMsg
                    |> Http.expectString
            }
