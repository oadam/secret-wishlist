module Pages.WishList exposing (Model, Msg, init, update, view)

import Api exposing (Present, PresentId, Token, User, UserId)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, for, hidden, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Session exposing (Session)
import String.Interpolate exposing (interpolate)
import TextHtml exposing (textHtml)


type Msg
    = GotPresents (Result Http.Error (List Present))


type alias Model =
    { session : Session
    , user : User
    , presents : Maybe (Result Http.Error (List Present))
    }


init : Session -> User -> (Msg -> msg) -> ( Model, Cmd msg )
init session user wishlistMsg =
    ( { session = session
      , user = user
      , presents = Nothing
      }
    , Api.getPresents session.token user.user_id (wishlistMsg << GotPresents)
    )


update : Msg -> Model -> (Msg -> msg) -> ( Model, Cmd msg )
update msg model wishlistMsg =
    case msg of
        GotPresents result ->
            ( { model | presents = Just result }
            , Cmd.none
            )


viewPresent : Model -> (Msg -> msg) -> Present -> Html msg
viewPresent model wishListMsg present =
    div [] [ text present.subject ]


viewPresents : Model -> (Msg -> msg) -> List (Html msg)
viewPresents model wishListMsg =
    case model.presents of
        Nothing ->
            [ p [] [ text "chargement en cours..." ] ]

        Just (Err _) ->
            [ div [ class "alert alert-danger" ] [ text "une erreur s'est produite sur le serveur" ] ]

        Just (Ok presents) ->
            List.map (viewPresent model wishListMsg) presents


view : Model -> (Msg -> msg) -> List (Html msg)
view model wishListMsg =
    [div []
        (h1 [ class "h3 mb-3 font-weight-normal" ] [ text (interpolate "Liste de {0}" [ model.user.name ]) ]
            :: viewPresents model wishListMsg
        )]
