module Api exposing (Token, Present, PresentId, User, UserId, login, getUsers, getPresents)

import Http
import Json.Decode exposing (Decoder, field, list, map4, map2, map, string, int)
import Process
import Task

type UserId = UserId Int

type alias User =
    { user_id: UserId
    , name: String}
type PresentId = PresentId Int
type alias Present =
    { present_id : PresentId
    , user_id : UserId
    , subject : String
    , content : String
    }

type Token = Token Bool String

mockHttpGet : (Result Http.Error a -> msg) -> a -> Cmd msg
mockHttpGet toMsg result =
    Process.sleep 1500
        |> Task.andThen (\_ -> Task.succeed (Ok result))
        |> Task.perform toMsg


login : String -> String -> (Result Http.Error Token -> msg) -> Cmd msg
login username password toMsg =
    if username == "demo" then
        mockHttpGet toMsg <| Token True "mockToken"

    else
        Http.get
            { url = "/api/login/"
            , expect =
                Http.expectJson toMsg (map (Token False) string)
            }

userDecoder : Decoder User
userDecoder =
    map2 User
        (field "user_id" (map UserId int))
        (field "name" string)

getUsers : Token -> (Result Http.Error (List User) -> msg) -> Cmd msg
getUsers (Token demo token) toMsg =
    if demo then
        mockHttpGet toMsg <| demoUsers

    else
        Http.get
            { url = "/api/login/"
            , expect =
                Http.expectJson toMsg (list userDecoder)
            }

presentDecoder : Decoder Present
presentDecoder =
    map4 Present
        (field "present_id" (map PresentId int))
        (field "user_id" (map UserId int))
        (field "subject" string)
        (field "content" string)


getPresents : Token -> UserId -> (Result Http.Error (List Present) -> msg) -> Cmd msg
getPresents (Token demo token) userName toMsg =
    if demo then
        mockHttpGet toMsg <| demoPresents

    else
        Http.get
            { url = "/api/login/"
            , expect =
                Http.expectJson toMsg (list presentDecoder)
            }

demoUsers : List User
demoUsers =
    [ {user_id= UserId 1
    , name= "papa"}
    ,{user_id= UserId 2
    , name= "maman"}
    ,{user_id= UserId 3
    , name= "bebe"}]


demoPresents : List Present
demoPresents =
    [ { present_id = PresentId 1
      , user_id = UserId 1
      , subject = "bd fantaisy"
      , content = "une bd comme on les aimes"
      }
    ]