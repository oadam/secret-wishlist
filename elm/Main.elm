module Main exposing (main)

import Help
import Api exposing (Present, Token)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Debug
import Html exposing (Html, a, br, button, div, footer, form, h1, h2, h3, h4, header, i, img, input, label, li, main_, nav, p, select, span, text, textarea, ul)
import Html.Attributes exposing (classList, alt, for, placeholder, value, attribute, class, hidden, href, id, src, title, type_)
import Html.Events exposing (onClick)
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


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( {page=
    Login { credentials = ( "", "" ), state = None }
    , help=False}
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
       ToggleHelp -> ({model|help=not model.help}, Cmd.none)
       _ -> Debug.todo "update"


forkMe =
    textHtml """<a href="https://github.com/oadam/secret-wishlist"><img alt="Fork me on GitHub" data-canonical-src="https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png" style="position: absolute; top: 0; right: 0; border: 0;" src="https://camo.githubusercontent.com/38ef81f8aca64bb9a64448d0d70f1308ef5341ab/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f6461726b626c75655f3132313632312e706e67"></a>"""



--case model of
--Login credentials loginState ->
--Debug.todo "update"
--App loggedState pendingModifications ->
--Debug.todo "update"


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
  form [ class "form-signin", class "text-center" ]
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
    Debug.todo "appMain"

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
