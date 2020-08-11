module Login exposing (..)

import Api
import Debug
import Html exposing (..)
import Html.Attributes exposing (class, hidden, id, required, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import TextHtml exposing (textHtml)


type Msg
    = Submit
    | UpdateLogin String
    | UpdatePassword String
    | GotAuth (Result Http.Error Api.Token)


type State
    = None
    | Loading
    | Error
    | Success


type alias Model =
    { login : String
    , password : String
    , user : String
    , state : State
    }


init : Model
init =
    { login = ""
    , password = ""
    , state = None
    , user = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAuth result ->
            case result of
                Ok token ->
                    ( { model | state = Success }, Cmd.none )

                Err _ ->
                    ( { model | state = Error }, Cmd.none )

        UpdateLogin login ->
            ( { model | login = login, state = None }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password, state = None }, Cmd.none )

        Submit ->
            ( { model | state = Loading }, Api.authenticate ( model.login, model.password ) GotAuth )


view : Model -> Html Msg
view model =
    let
        alert =
            case model.state of
                None ->
                    ""

                Loading ->
                    "<div class='alert alert-info'>Vérification de l'événement...</div>"

                Success ->
                    "<div class='alert alert-success'>Evénement vérifié !</div>"

                Error ->
                    "<div class='alert alert-danger'>Identifiant/Mot de passe invalides</div>"
    in
    form [ class "form", onSubmit Submit ]
        ([ h2 [] [ text "Connexion" ]
         , div [ class "form-group" ]
            [ label []
                [ text "Nom de l'évènement" ]
            , input [ class "form-control", value model.login, required True, onInput UpdateLogin ]
                []
            ]
         , div [ class "form-group" ]
            [ label []
                [ text "Mot de passe de l'évènement" ]
            , input [ class "form-control", value model.password, type_ "password", required True, onInput UpdatePassword ]
                []
            ]
         ]
            ++ textHtml alert
            ++ [ div [ class "form-group", hidden (model.state == Success) ]
                    [ button [ class "btn btn-primary", type_ "submit" ]
                        [ text "Connexion" ]
                    ]
               ]
        )
