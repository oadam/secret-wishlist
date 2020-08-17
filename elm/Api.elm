module Api exposing (Token, login, getPresents)

import Http
import Json.Decode exposing (Decoder, field, list, map4, map2, string, int)
import Process
import Task
import Debug
import Demo
import Present exposing (Present)


type alias Token =
    { demo : Bool
    , token : String
    , users : List String
    }


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


login : String -> String -> (Result Http.Error Token -> msg) -> Cmd msg
login username password toMsg =
    if username == "demo" then
        mockHttpGet toMsg <| Token True "mockToken" Demo.users

    else
        Http.get
            { url = "/api/login/"
            , expect =
                Http.expectJson toMsg loginDecoder
            }

presentDecoder : Decoder Present
presentDecoder =
    map4 Present
        (field "id" int)
        (field "user" string)
        (field "subject" string)
        (field "content" string)


getPresents : Token -> String -> (Result Http.Error (List Present) -> msg) -> Cmd msg
getPresents token userName toMsg =
    if token.demo then
        mockHttpGet toMsg <| Demo.presents

    else
        Http.get
            { url = "/api/login/"
            , expect =
                Http.expectJson toMsg (list presentDecoder)
            }