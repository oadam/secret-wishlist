module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Html exposing (Html, button, div, h3, span, text)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import String.Interpolate exposing (interpolate)
import Url exposing (Url)


type alias Present =
    { id : Int
    , title : String
    , body : String
    }


type alias Model =
    { presents : List Present
    , navKey : Key
    }


demoPresents : List Present
demoPresents =
    [ { id = 1, title = "salut", body = "salut toi" } ]


type Msg
    = Noop


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { presents = demoPresents, navKey = key }
    , Cmd.none
      --, Browser.Navigation.pushUrl key "/0/"
    )



-- updateUrl : Int -> Cmd Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = interpolate "Liste de {0}" [ "josé" ]
    , body =
        [ div []
            [ Keyed.ul [] (List.map viewKeyedPresent model.presents)
            ]
        ]
    }


viewPresent : Present -> Html Msg
viewPresent present =
    div []
        [ h3 []
            [ text present.title ]
        , span
            []
            [ text present.body ]
        ]


viewKeyedPresent : Present -> ( String, Html Msg )
viewKeyedPresent present =
    ( String.fromInt present.id, lazy viewPresent present )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


onUrlRequest : UrlRequest -> Msg
onUrlRequest request =
    Noop


onUrlChange : Url -> Msg
onUrlChange url =
    Noop


main =
    Browser.application { init = init, update = update, view = view, subscriptions = subscriptions, onUrlRequest = onUrlRequest, onUrlChange = onUrlChange }
