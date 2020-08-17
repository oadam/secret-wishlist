module Pages.Login exposing (Model, Msg, init, update, view)

import Api exposing (Token)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, for, hidden, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import TextHtml exposing (textHtml)


type State
    = None
    | Submitted
    | Failed
    | PickingUser Token (Maybe String)


type Msg
    = SubmitCredentials
    | UpdateUsername String
    | UpdatePassword String
    | UpdatePickedUser String
    | GotAuth (Result Http.Error Api.Token)


type alias Model =
    { username : String
    , password : String
    , state : State
    }


init : Model
init =
    { username = ""
    , password = ""
    , state = None
    }


update : Msg -> Model -> (Msg -> msg) -> ( Model, Cmd msg )
update msg model loginMsg =
    case msg of
        UpdateUsername u ->
            ( { model | username = u }
            , Cmd.none
            )

        UpdatePassword p ->
            ( { model | password = p }
            , Cmd.none
            )

        UpdatePickedUser p ->
            case model.state of
                PickingUser token _ ->
                    ( { model | state = PickingUser token (Just p) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SubmitCredentials ->
            ( { model | state = Submitted }
            , Api.login model.username model.password (loginMsg << GotAuth)
            )

        GotAuth (Ok token) ->
            ( { model | state = PickingUser token Nothing }
            , Cmd.none
            )

        GotAuth (Err _) ->
            ( { model | state = Failed }
            , Cmd.none
            )


alert : State -> List (Html msg)
alert state =
    textHtml <|
        case state of
            None ->
                ""

            Submitted ->
                "<div class='alert alert-info'>Vérification de l'événement...</div>"

            PickingUser _ _ ->
                ""

            Failed ->
                "<div class='alert alert-danger'>Identifiant/Mot de passe invalides</div>"


stringToOption : String -> Html msg
stringToOption s =
    option [ value s ] [ text s ]


view : Model -> (Msg -> msg) -> (Token -> String -> msg) -> Html msg
view model loginMsg startSession =
    let
        ( maybeUsers, submitMsg ) =
            case model.state of
                PickingUser token (Just u) ->
                    ( Just token.users, startSession token u )

                PickingUser token Nothing ->
                    ( Just token.users, startSession token "bogusUser !!!" )

                _ ->
                    ( Nothing, loginMsg SubmitCredentials )
    in
    [ h1 [ class "h3 mb-3 font-weight-normal" ]
        [ text "Connexion" ]
    , label [ class "sr-only", for "inputEvent" ]
        [ text "Nom de l'événement" ]
    , input [ attribute "autofocus" "", id "inputEvent", value model.username, onInput (loginMsg << UpdateUsername), class "form-control", placeholder "Nom de l'événement", attribute "required" "" ]
        []
    , label [ class "sr-only", for "inputPassword" ]
        [ text "Mot de passe" ]
    , input [ class "form-control", id "inputPassword", value model.password, onInput (loginMsg << UpdatePassword), placeholder "Mot de passe", attribute "required" "", type_ "password" ]
        []
    ]
        ++ alert model.state
        ++ [ div [ class "form-group", hidden (maybeUsers == Nothing) ]
                [ label [ for "inputPickedUser" ]
                    [ text "Se connecter en tant que :" ]
                , select [ id "inputPickedUser", class "form-control", onInput (loginMsg << UpdatePickedUser) ] (Maybe.withDefault [] maybeUsers |> List.map stringToOption)
                ]
           , button [ class "btn btn-lg btn-primary btn-block", type_ "submit" ]
                [ text "Connexion" ]
           ]
        |> form [ class "form-signin", class "text-center", onSubmit submitMsg ]
