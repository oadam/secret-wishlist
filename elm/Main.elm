module Main exposing (main)

import Api exposing (Credentials, Present, Token)
import Browser
import Debug
import Help
import Html exposing (Html, a, br, button, div, footer, form, h1, h2, h3, h4, header, i, img, input, label, li, main_, nav, option, p, select, span, text, textarea, ul)
import Html.Attributes exposing (alt, attribute, class, classList, for, hidden, href, id, placeholder, src, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Lazy exposing (lazy)
import Http
import PendingModification exposing (PendingModification)
import String.Interpolate exposing (interpolate)
import TextHtml exposing (textHtml)


type LoginState
    = None
    | Submitted
    | Failed
    | PickingUser { token : Token, user : Maybe String }


type Session
    = Session { token : Token, user : String }


type LoggedState
    = ViewList { session : Session, user : String, presents : Maybe (List Present) }
    | EditPresent { session : Session, present : Present }


type Page
    = Login { credentials : Credentials, state : LoginState }
    | App { state : LoggedState, pendingModifications : List PendingModification }


type alias Model =
    { page : Page, help : Bool }


type Msg
    = Noop
    | ToggleHelp
    | SubmitLogin
    | UpdateLoginUsername String
    | UpdateLoginPassword String
    | UpdateLoginPickedUser String
    | Logout
    | GotAuth (Result Http.Error Token)


init : () -> ( Model, Cmd Msg )
init flags =
    ( { page =
            Login { credentials = Credentials "" "", state = None }
      , help = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model.page ) of
        ( ToggleHelp, _ ) ->
            ( { model | help = not model.help }, Cmd.none )

        ( UpdateLoginUsername u, Login login ) ->
            ( { model | page = Login { login | credentials = Credentials u login.credentials.password } }
            , Cmd.none
            )

        ( UpdateLoginPassword p, Login login ) ->
            ( { model | page = Login { login | credentials = Credentials login.credentials.username p } }
            , Cmd.none
            )

        ( UpdateLoginPickedUser p, Login login ) ->
            case login.state of
                PickingUser { token } ->
                    ( { model | page = Login { login | state = PickingUser { token = token, user = Just p } } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ( SubmitLogin, Login login ) ->
            case login.state of
                PickingUser { token, user } ->
                    case user of
                        Nothing ->
                            ( model, Cmd.none )

                        Just u ->
                            ( { model | page = App { pendingModifications = [], state = ViewList { user = u, presents = Nothing, session = Session { token = token, user = u } } } }
                            , Cmd.none
                            )

                _ ->
                    ( { model | page = Login { login | state = Submitted } }
                    , Api.login login.credentials GotAuth
                    )

        ( GotAuth (Ok token), Login login ) ->
            ( { model | page = Login { login | state = PickingUser { token = token, user = Nothing } } }
            , Cmd.none
            )

        ( GotAuth (Err _), Login login ) ->
            ( { model | page = Login { login | state = Failed } }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


loggedTitle : LoggedState -> String
loggedTitle state =
    case state of
        ViewList { user } ->
            user

        EditPresent { present } ->
            interpolate "Liste de {0}" [ present.user ]


windowTitle : Page -> String
windowTitle page =
    case page of
        Login _ ->
            "Secret Wishlist"

        App { state } ->
            loggedTitle state


loginAlert : LoginState -> List (Html msg)
loginAlert state =
    textHtml <|
        case state of
            None ->
                ""

            Submitted ->
                "<div class='alert alert-info'>Vérification de l'événement...</div>"

            PickingUser _ ->
                ""

            Failed ->
                "<div class='alert alert-danger'>Identifiant/Mot de passe invalides</div>"


stringToOption : String -> Html msg
stringToOption s =
    option [ value s ] [ text s ]


loginMain : { credentials : Credentials, state : LoginState } -> Html Msg
loginMain { credentials, state } =
    let
        userPicker =
            case state of
                PickingUser { token } ->
                    [ div [ class "form-group" ]
                        [ label [ for "inputPickedUser" ]
                            [ text "Se connecter en tant que :" ]
                        , select [ id "inputPickedUser", class "form-control", onInput UpdateLoginPickedUser ] (List.map stringToOption token.users)
                        ]
                    ]

                _ ->
                    []
    in
    [ h1 [ class "h3 mb-3 font-weight-normal" ]
        [ text "Connexion" ]
    , label [ class "sr-only", for "inputEvent" ]
        [ text "Nom de l'événement" ]
    , input [ attribute "autofocus" "", id "inputEvent", value credentials.username, onInput UpdateLoginUsername, class "form-control", placeholder "Nom de l'événement", attribute "required" "" ]
        []
    , label [ class "sr-only", for "inputPassword" ]
        [ text "Mot de passe" ]
    , input [ class "form-control", id "inputPassword", value credentials.password, onInput UpdateLoginPassword, placeholder "Mot de passe", attribute "required" "", type_ "password" ]
        []
    ]
        ++ loginAlert state
        ++ userPicker
        ++ [ button [ class "btn btn-lg btn-primary btn-block", type_ "submit" ]
                [ text "Connexion" ]
           ]
        |> form [ class "form-signin", class "text-center", onSubmit SubmitLogin ]


appMain : { state : LoggedState, pendingModifications : List PendingModification } -> Html Msg
appMain { state, pendingModifications } =
    div [] []


viewMain : Page -> Html Msg
viewMain page =
    case page of
        Login state ->
            loginMain state

        App state ->
            appMain state


view : Model -> Browser.Document Msg
view { page, help } =
    { title = windowTitle page
    , body =
        (if help then
            Help.modal ToggleHelp

         else
            []
        )
            ++ [ div [ class "cover-container d-flex w-100 h-100 p-3 mx-auto flex-column" ]
                    [ header [ class "masthead mb-auto" ]
                        [ div [ class "inner" ]
                            [ h3 [ class "masthead-brand" ]
                                [ text "Secret Wishlist" ]
                            , nav [ class "nav nav-masthead justify-content-center" ]
                                [ a [ classList [ ( "nav-link", True ), ( "active", help ) ], onClick ToggleHelp ]
                                    [ text "Aide" ]
                                , a [ class "nav-link", href "#" ]
                                    [ text "Features" ]
                                , a [ class "nav-link", href "#" ]
                                    [ text "Contact" ]
                                ]
                            ]
                        ]
                    , main_ [ class "inner cover", attribute "role" "main" ]
                        [ viewMain page ]
                    , footer [ class "mastfoot mt-auto" ]
                        [ div [ class "inner" ]
                            [ p []
                                [ text "Cover template for "
                                , a [ href "https://getbootstrap.com/" ]
                                    [ text "Bootstrap" ]
                                , text ", by "
                                , a [ href "https://twitter.com/mdo" ]
                                    [ text "@mdo" ]
                                , text "."
                                ]
                            ]
                        ]
                    ]
               ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }
