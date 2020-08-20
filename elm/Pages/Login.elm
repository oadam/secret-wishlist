module Pages.Login exposing (Model, Msg, init, update, view)

import Api exposing (Token, User)
import Array exposing (Array)
import Html exposing (Html, button, div, form, h1, input, label, option, select, text)
import Html.Attributes exposing (attribute, class, disabled, for, id, placeholder, selected, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Session exposing (Session)
import TextHtml exposing (textHtml)


type State
    = None
    | Submitted
    | Failed
    | LoadingUsers Token
    | PickingUser Token (Array User) (Maybe User)


type Msg
    = Noop
    | SubmitCredentials
    | UpdateUsername String
    | UpdatePassword String
    | UpdatePickedUser (Maybe User)
    | GotAuth (Result Http.Error Api.Token)
    | GotUsers (Result Http.Error (List User))


type Model msg = Model
    { username : String
    , password : String
    , state : State
    , loginMsg : (Msg -> msg) 
    , sessionMsg : (Session -> msg)
    }


init : (Msg -> msg) -> (Session -> msg) -> Model msg
init loginMsg sessionMsg = Model
    { username = ""
    , password = ""
    , state = None
    , loginMsg = loginMsg
    , sessionMsg = sessionMsg
    }


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg (Model model) =
    case ( model.state, msg ) of
        ( _, Noop ) ->
            ( Model model, Cmd.none )

        ( _, UpdateUsername u ) ->
            ( Model { model | username = u }
            , Cmd.none
            )

        ( _, UpdatePassword p ) ->
            ( Model { model | password = p }
            , Cmd.none
            )

        ( PickingUser token users _, UpdatePickedUser picked ) ->
            ( Model { model | state = PickingUser token users picked }
            , Cmd.none
            )

        ( _, SubmitCredentials ) ->
            ( Model { model | state = Submitted }
            , Api.login model.username model.password (model.loginMsg << GotAuth)
            )

        ( _, GotAuth (Ok token) ) ->
            ( Model { model | state = LoadingUsers token }
            , Api.getUsers token (model.loginMsg << GotUsers)
            )

        ( _, GotAuth (Err _) ) ->
            ( Model { model | state = Failed }
            , Cmd.none
            )

        ( LoadingUsers token, GotUsers (Ok users) ) ->
            ( Model { model | state = PickingUser token (Array.fromList users) Nothing }
            , Cmd.none
            )

        ( _, GotUsers (Err _) ) ->
            ( Model { model | state = Failed }
            , Cmd.none
            )

        ( _, _ ) ->
            ( Model model, Cmd.none )


alert : State -> List (Html msg)
alert state =
    textHtml <|
        case state of
            None ->
                ""

            Submitted ->
                "<div class='alert alert-info'>Vérification de l'événement...</div>"

            LoadingUsers _ ->
                "<div class='alert alert-info'>Chargement des utilisateurs...</div>"

            PickingUser _ _ _ ->
                ""

            Failed ->
                "<div class='alert alert-danger'>Identifiant/Mot de passe invalides</div>"


userSelect : Model msg -> List (Html msg)
userSelect (Model model) =
    case model.state of
        PickingUser _ users picked_user ->
            [ div [ class "form-group" ]
                [ label [ for "inputPickedUser" ] [ text "Se connecter en tant que :" ]
                , select [ id "inputPickedUser", class "form-control", onInput (model.loginMsg << UpdatePickedUser << userIdFromOption users) ]
                    (option [ disabled True, selected (picked_user == Nothing) ] [ text "\u{00A0}" ]
                        :: (Array.toList users |> List.indexedMap (userToOption picked_user))
                    ) ]
            ]

        _ ->
            []


userToOption : (Maybe User) -> Int -> User -> Html msg
userToOption picked_user index user =
    let userSelected = case picked_user of
            Nothing -> False
            Just u -> u.user_id == user.user_id
        in
            option [ value (String.fromInt index), selected userSelected ] [ text user.name ]


userIdFromOption : Array User -> String -> Maybe User
userIdFromOption users index =
    String.toInt index
        |> Maybe.andThen (\i -> Array.get i users)


view : Model msg -> List (Html msg)
view (Model model) =
    let
        (submitDisabled, submitMsg) =
            case model.state of
                PickingUser token users (Just u) ->
                    (False, model.sessionMsg (Session token (Array.toList users) u))

                PickingUser _ _ Nothing ->
                    (True, model.loginMsg Noop)

                _ ->
                    (False, model.loginMsg SubmitCredentials)
    in
    [ h1 [ class "h3 mb-3 font-weight-normal" ]
        [ text "Connexion" ]
    , label [ class "sr-only", for "inputEvent" ]
        [ text "Nom de l'événement" ]
    , input [ attribute "autofocus" "", id "inputEvent", value model.username, onInput (model.loginMsg << UpdateUsername), class "form-control", placeholder "Nom de l'événement", attribute "required" "" ]
        []
    , label [ class "sr-only", for "inputPassword" ]
        [ text "Mot de passe" ]
    , input [ class "form-control", id "inputPassword", value model.password, onInput (model.loginMsg << UpdatePassword), placeholder "Mot de passe", attribute "required" "", type_ "password" ]
        []
    ]
        ++ alert model.state
        ++ userSelect (Model model)
        ++ [ button [ disabled submitDisabled, class "btn btn-lg btn-primary btn-block", type_ "submit" ]
                [ text "Connexion" ]
           ]
        |> form [ disabled submitDisabled, class "form-signin", class "text-center", onSubmit submitMsg ]
        |> List.singleton
