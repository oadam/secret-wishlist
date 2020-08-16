module Main exposing (main)

import Help
import Http
import Api exposing (Present, Token)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Debug
import Html exposing (Html, a, br, button, div, footer, form, h1, h2, h3, h4, header, i, img, input, label, li, main_, nav, p, select, span, text, textarea, ul)
import Html.Attributes exposing (classList, alt, for, placeholder, value, attribute, class, hidden, href, id, src, title, type_)
import Html.Events exposing (onClick, onSubmit)
import Html.Lazy exposing (lazy)
import PendingModification exposing (PendingModification)
import String.Interpolate exposing (interpolate)
import TextHtml exposing (textHtml)
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query


type alias Credentials =
    ( String, String )


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

type alias Model = {page: Page, help: Bool}

type Msg
    = Noop
    | ToggleHelp
    | SubmitLogin
    | Logout
    | GotAuth (Result Http.Error Token)


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( {page=
    Login { credentials = ( "", "" ), state = None }
    , help=False}
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case (message, model.page) of
       (ToggleHelp, _) ->
        ({model|help=not model.help}, Cmd.none)
       (SubmitLogin, Login state) ->
          ({model|page=Login {state|state=Submitted}} , Api.login state.credentials GotAuth )
       _ ->
          (model, Cmd.none)

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

loginMain : {credentials: Credentials, state: LoginState} -> Html Msg
loginMain {credentials, state} =
  form [ class "form-signin", class "text-center", onSubmit SubmitLogin ]
    [ 
     h1 [ class "h3 mb-3 font-weight-normal" ]
        [ text "Connexion" ]
    , label [ class "sr-only", for "inputEmail" ]
        [ text "Nom de l'événement" ]
    , input [ attribute "autofocus" "", class "form-control", id "inputEmail", placeholder "Nom de l'événement", attribute "required" "", type_ "email" ]
        []
    , label [ class "sr-only", for "inputPassword" ]
        [ text "Mot de passe" ]
    , input [ class "form-control", id "inputPassword", placeholder "Mot de passe", attribute "required" "", type_ "password" ]
        []
    , button [ class "btn btn-lg btn-primary btn-block", type_ "submit" ]
        [ text "Connexion" ]
    ] 

appMain : {state: LoggedState, pendingModifications: List PendingModification} -> Html Msg
appMain {state, pendingModifications} =
    div [] []

viewMain : Page -> Html Msg
viewMain page =
    case page of
        Login state ->
            loginMain state

        App state ->
            appMain state

view : Model -> Browser.Document Msg
view {page, help} =
    { title = windowTitle page
    , body =
        (if help then Help.modal ToggleHelp else []) ++
        [ div [ class "cover-container d-flex w-100 h-100 p-3 mx-auto flex-column" ]
            [ header [ class "masthead mb-auto" ]
                [ div [ class "inner" ]
                    [ h3 [ class "masthead-brand" ]
                        [ text "Secret Wishlist" ]
                    , nav [ class "nav nav-masthead justify-content-center" ]
                        [ a [ classList [("nav-link", True), ("active", help)], onClick ToggleHelp ]
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


onUrlRequest : UrlRequest -> Msg
onUrlRequest request =
    Noop


onUrlChange : Url -> Msg
onUrlChange url =
    Noop


main =
    Browser.application { init = init, update = update, view = view, subscriptions = subscriptions, onUrlRequest = onUrlRequest, onUrlChange = onUrlChange }
