module Api exposing (Present, PresentId, Token, User, UserId, getPresents, addPresent, updatePresent, getUsers, login, userIdFromString, userIdToString)

import Http
import Json.Decode exposing (Decoder, field, float, int, list, map, map2, map8, nullable, string)
import Process
import Task


type UserId
    = UserId Int


type alias User =
    { user_id : UserId
    , name : String
    }


type PresentId
    = PresentId Int


type alias Present =
    { id : PresentId
    , to : UserId
    , title : String
    , description : String
    , createdBy : UserId
    , offeredBy : Maybe UserId
    , deletedBy : Maybe UserId
    , sort : Float
    }


type Token
    = Token Bool String


userIdToString : UserId -> String
userIdToString (UserId v) =
    String.fromInt v


userIdFromString : String -> Maybe UserId
userIdFromString v =
    Maybe.map UserId (String.toInt v)


mockHttpGet : (Result Http.Error a -> msg) -> a -> Cmd msg
mockHttpGet toMsg result =
    Process.sleep 500
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
    map8 Present
        (field "id" (map PresentId int))
        (field "to" (map UserId int))
        (field "title" string)
        (field "description" string)
        (field "createdBy" (map UserId int))
        (field "offeredBy" (map (Maybe.map UserId) (nullable int)))
        (field "deletedBy" (map (Maybe.map UserId) (nullable int)))
        (field "sort" float)


getPresents : Token -> UserId -> (Result Http.Error (List Present) -> msg) -> Cmd msg
getPresents (Token demo token) user_id toMsg =
    if demo then
        mockHttpGet toMsg <| List.filter (\p -> p.to == user_id) demoPresents

    else
        Http.get
            { url = "/api/login/"
            , expect =
                Http.expectJson toMsg (list presentDecoder)
            }

addPresent : Token -> {title: String, description: String, createdBy: UserId, to: UserId} -> (Result Http.Error Present -> msg) -> Cmd msg
addPresent (Token demo token) present toMsg =
    if demo then
        mockHttpGet toMsg { id = PresentId 1000
      , to = present.to
      , title = present.title
      , description = present.description
      , createdBy = present.createdBy
      , offeredBy = Nothing
      , deletedBy = Nothing
      , sort = 0
    }

    else
        Http.get
            { url = "/api/login/"
            , expect =
                Http.expectJson toMsg presentDecoder
            }
updatePresent : Token -> Present -> (Result Http.Error Present -> msg) -> Cmd msg
updatePresent (Token demo token) present toMsg =
    if demo then
        mockHttpGet toMsg present

    else
        Http.get
            { url = "/api/login/"
            , expect =
                Http.expectJson toMsg presentDecoder
            }


demoUsers : List User
demoUsers =
    [ { user_id = UserId 1
      , name = "papa"
      }
    , { user_id = UserId 2
      , name = "maman"
      }
    , { user_id = UserId 3
      , name = "bebe"
      }
    ]


demoPresents : List Present
demoPresents =
    [ { id = PresentId 1
      , to = UserId 1
      , title = "bd fantaisy"
      , description = "une bande-dessinée un peu sympa parlant de fantaisy"
      , createdBy = UserId 1
      , offeredBy = Nothing
      , deletedBy = Nothing
      , sort = 0
      }
      ,{ id = PresentId 2
      , to = UserId 1
      , title = "mug floral"
      , description = "pour boire mon café, un design plutôt tendance"
      , createdBy = UserId 1
      , offeredBy = Just (UserId 2)
      , deletedBy = Nothing
      , sort = 0.5
      }
    ]
