module Pages.WishList exposing (Model, Msg, init, update, view)

import Session exposing (Session)
import Api exposing (Token)
import Present exposing (Present)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, for, hidden, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import TextHtml exposing (textHtml)

type Msg
    = GotPresents (Result Http.Error (List Present))

type alias Model =
    { session : Session
    , user : String
    , presents : Maybe (Result Http.Error (List Present)) }


init : Session -> String -> (Msg -> msg) -> (Model, Cmd msg)
init session user wishlistMsg =
    ({ session = session
    , user = user
    , presents = Nothing
    }
    , Api.getPresents session.token user (wishlistMsg << GotPresents)
    )


update : Msg -> Model -> (Msg -> msg) -> ( Model, Cmd msg )
update msg model wishlistMsg =
    case msg of
        GotPresents result ->
            ( { model | presents = Just result }
            , Cmd.none
            )


view : Model -> (Msg -> msg) -> Html msg
view model wishListMsg =
    h1 [ class "h3 mb-3 font-weight-normal" ]
        [ text ("Salut" ++ model.user) ]
