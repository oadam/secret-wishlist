module Login exposing (..)

import Api
import Debug
import Html exposing (..)
import Html.Attributes exposing (class, hidden, id, required, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import TextHtml exposing (textHtml)


type Msg
    = Submit
    | UpdateLogin String
    | UpdatePassword String


type State
    = None
    | Loading
    | Error
    | Success


type alias Model =
    { login : String
    , password : String
    , user : String
    , allUsers : List String
    , state : State
    }


init : Bool -> Model
init demo =
    let
        demoString =
            if demo then
                "demo"

            else
                ""
    in
    { login = demoString
    , password = demoString
    , state = None
    , allUsers = []
    , user = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        UpdateLogin login ->
            ( { model | login = login }, Cmd.none )

        Submit ->
            ( { model | state = Loading }, Cmd.none )


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
            ++ [ div [ class "form-group" ]
                    [ button [ class "btn btn-primary", hidden (model.state == Success), type_ "submit" ]
                        [ text "Connexion" ]
                    ]
               ]
        )
