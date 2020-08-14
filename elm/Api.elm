module Api exposing (Present, Token, login)

import Http
import Json.Decode exposing (Decoder, field, list, map2, string)
import Process
import Task


type alias Present =
    { id : String
    , user : String
    , subject : String
    , content : String
    }


type alias Token =
    { demo : Bool
    , token : String
    , users : List String
    }


demoUsers : List String
demoUsers =
    [ "papa", "maman", "tonton" ]


mockHttpGet : (Result Http.Error a -> msg) -> a -> Cmd msg
mockHttpGet toMsg result =
    Process.sleep 1500
        |> Task.andThen (\_ -> Task.succeed (Ok result))
        |> Task.perform toMsg


loginDecoder : Decoder Token
loginDecoder =
    map2 (Token False)
        (field "token" string)
        (field "users" <| list <| string)


login : ( String, String ) -> (Result Http.Error Token -> msg) -> Cmd msg
login ( username, _ ) toMsg =
    if username == "demo" then
        mockHttpGet toMsg <| Token True "mockToken" demoUsers

    else
        Http.get
            { url = "/api/login/"
            , expect =
                Http.expectJson toMsg loginDecoder
            }
