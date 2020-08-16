module Main exposing (main)

import Api exposing (Present, Token)
import Browser
import Debug
import Help
import Html exposing (Html, a, br, button, div, footer, form, h1, h2, h3, h4, header, i, img, input, label, li, main_, nav, option, p, select, span, text, textarea, ul)
import Html.Attributes exposing (alt, attribute, class, classList, for, hidden, href, id, placeholder, src, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Lazy exposing (lazy)
import Http
import Login
import PendingModification exposing (PendingModification)
import String.Interpolate exposing (interpolate)
import TextHtml exposing (textHtml)


type alias Session =
    { token : Token, user : String }


type LoggedState
    = ViewList { session : Session, user : String, presents : Maybe (List Present) }
    | EditPresent { session : Session, present : Present }


type Page
    = Login Login.Model
    | App { state : LoggedState, pendingModifications : List PendingModification }


type alias Model =
    { page : Page, help : Bool }


type Msg
    = ToggleHelp
    | StartSession Token String
    | LoginMsg Login.Msg
    | Logout


init : () -> ( Model, Cmd Msg )
init flags =
    ( { page =
            Login Login.init
      , help = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model.page ) of
        ( ToggleHelp, _ ) ->
            ( { model | help = not model.help }, Cmd.none )

        ( LoginMsg msg, Login login ) ->
            let
                ( loginModel, cmd ) =
                    Login.update msg login LoginMsg
            in
            ( { model | page = Login loginModel }, cmd )

        ( StartSession token username, Login _ ) ->
            ( { model | page = App { pendingModifications = [], state = ViewList { session = Session token username, user = username, presents = Nothing } } }, Cmd.none )

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


appMain : { state : LoggedState, pendingModifications : List PendingModification } -> Html Msg
appMain { state, pendingModifications } =
    div [] []


viewMain : Page -> Html Msg
viewMain page =
    case page of
        Login login ->
            Login.view login LoginMsg StartSession

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
