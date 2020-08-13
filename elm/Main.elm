module Main exposing (main)

import Api exposing (Present, Token, Username)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Debug
import Html exposing (Html, a, br, button, div, footer, form, h1, h2, h3, h4, i, img, input, label, li, p, select, span, text, textarea, ul)
import Html.Attributes exposing (alt, attribute, class, hidden, href, id, src, title, type_)
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
    | PickingUser Token (Maybe Username)


type Session
    = Session Token Username


type LoggedState
    = ViewList Session Username (Maybe (List Present))
    | EditPresent Session Present


type Model
    = Login Credentials LoginState
    | Logged LoggedState (List PendingModification)


type Msg
    = Noop
    | SubmitLogin
    | Logout


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Login ( "", "" ) None
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    Debug.todo "update"


forkMe =
    textHtml """<a href="https://github.com/oadam/secret-wishlist"><img alt="Fork me on GitHub" data-canonical-src="https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png" style="position: absolute; top: 0; right: 0; border: 0;" src="https://camo.githubusercontent.com/38ef81f8aca64bb9a64448d0d70f1308ef5341ab/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f6461726b626c75655f3132313632312e706e67"></a>"""


view : Model -> Browser.Document Msg
view model =
    { title = interpolate "Liste de {0}" [ "josé" ]
    , body =
        [ footer [] (textHtml "<footer>Développé avec amour par <a href='https://github.com/oadam/'>oadam</a><br>Icons by <a href='http://glyphicons.com/'>Glyphicons</a><br></footer>")
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
