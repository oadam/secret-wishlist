module Main exposing (main)

import Api
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Html exposing (Html, a, br, button, div, footer, form, h1, h2, h3, h4, i, img, input, label, li, p, select, span, text, textarea, ul)
import Html.Attributes exposing (alt, attribute, class, hidden, href, id, src, title, type_)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Login
import Popups
import String.Interpolate exposing (interpolate)
import TextHtml exposing (textHtml)
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query


type alias Present =
    { id : Int
    , title : String
    , body : String
    }


type alias Model =
    { presents : List Present
    , navKey : Key
    , welcome : Bool
    , login : Login.Model
    , demo : Bool
    }


demoPresents : List Present
demoPresents =
    [ { id = 1, title = "salut", body = "salut toi" }
    , { id = 2, title = "hello", body = "salut toi" }
    , { id = 3, title = "yo", body = "salut toi" }
    , { id = 4, title = "lsdjflqksjf lqsjfdlkdsj", body = "salut toi" }
    , { id = 5, title = "qlksjflk", body = "salut toi" }
    ]


type Msg
    = Noop
    | PopupsMsg Popups.Msg
    | LoginMsg Login.Msg


init : { demo : Bool } -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { presents = demoPresents
      , navKey = key
      , welcome = True
      , login = Login.init flags.demo
      , demo = flags.demo
      }
    , Cmd.none
      --, Browser.Navigation.pushUrl key "/0/"
    )



-- updateUrl : Int -> Cmd Msg


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PopupsMsg Popups.HideWelcome ->
            ( { model | welcome = False }, Cmd.none )

        LoginMsg msg ->
            let
                ( loginModel, loginCmd ) =
                    Login.update msg model.login
            in
            ( { model | login = loginModel }, Cmd.map LoginMsg loginCmd )

        Noop ->
            ( model, Cmd.none )


forkMe =
    a [ href "https://github.com/oadam/dedguenodgo" ]
        [ img [ alt "Fork me on GitHub", attribute "data-canonical-src" "https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png", src "https://camo.githubusercontent.com/38ef81f8aca64bb9a64448d0d70f1308ef5341ab/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f6461726b626c75655f3132313632312e706e67", attribute "style" "position: absolute; top: 0; right: 0; border: 0;" ]
            []
        ]


view : Model -> Browser.Document Msg
view model =
    { title = interpolate "Liste de {0}" [ "josé" ]
    , body =
        List.map (\h -> Html.map PopupsMsg h) (Popups.welcome model.welcome)
            ++ [ h1 [] [ text "Dedguenodgo " ]
               , forkMe
               , div [ class "container" ]
                    [ Html.map LoginMsg (Login.view model.login) ]

               --                        , form [ class "form", attribute "data-bind" "visible: partyOk, submit: function() {$root.submitUser();}" ]
               --                            [ div [ class "form-group" ]
               --                                [ label []
               --                                    [ text "Utilisateur" ]
               --                                , select [ class "form-control", attribute "data-bind" "visible: users(), options: sortedUsers, value: user, optionsText:'name', optionsCaption: 'choisir...'" ]
               --                                    []
               --                                ]
               --                            , div [ class "form-group" ]
               --                                [ button [ class "btn btn-primary", attribute "data-bind" "enable: user", type_ "submit" ]
               --                                    [ text "Connexion" ]
               --                                , span []
               --                                    [ text "--- " ]
               --                                , button [ class "btn btn-xs btn-primary", attribute "data-bind" "click:function() {$root.addUser();}" ]
               --                                    [ text "Ajouter un utilisateur" ]
               --                                , button [ class "btn btn-xs btn-danger", attribute "data-bind" "visible: user, click:function() {$root.deleteUser();}" ]
               --                                    [ text "Supprimer cet utilisateur" ]
               --                                ]
               --                            ]
               --                        , div [ class "alert alert-info", attribute "data-bind" "visible: userActionLoading" ]
               --                            [ text "Appel du serveur..." ]
               --                        , div [ class "alert alert-success", attribute "data-bind" "visible: userActionOk" ]
               --                            [ text "OK !" ]
               --                        , div [ class "alert alert-danger", attribute "data-bind" "visible: userActionError" ]
               --                            [ text "Une erreur s'est produite sur le serveur" ]
               --                        ]
               --                    ]
               --                , div [ id "app" ]
               --                    [ div [ id "connected-user" ]
               --                        [ span [ attribute "data-bind" "text: !(loggedInUser()) ? '' : getUserName(loggedInUser())" ]
               --                            []
               --                        , br []
               --                            []
               --                        , a [ attribute "data-bind" "visible: loggedInUser, click: logout", href "#" ]
               --                            [ text "déconnexion" ]
               --                        ]
               --                    , div [ id "list-select" ]
               --                        [ text "Liste de :      "
               --                        , select [ attribute "data-bind" "options: lists(), value: selectedList, optionsValue: 'id', optionsText:'label'" ]
               --                            []
               --                        ]
               --                    , p [ class "jumbotron", attribute "data-bind" "visible: displayedPresents().length == 0" ]
               --                        [ text "Cette liste est vide ! Vous pouvez ajouter des idées de cadeaux à l'aide du bouton ci-dessous   " ]
               --                    , ul [ class "list-group", attribute "data-bind" "foreach: displayedPresents()", id "present-list" ]
               --                        [ li [ class "present list-group-item", attribute "data-bind" "event: { dblclick: function(){$root.editPresent($data);} }, css: { offered: $root.displayPresentAsOffered($data) }" ]
               --                            [ span [ class "buttons pull-right" ]
               --                                [ a [ attribute "data-bind" "click: function() { $root.togglePresentOffered($data); }, text: $root.displayPresentAsOffered($data) ? 'Dé-rayer' : 'Rayer', attr: { title: $root.displayPresentAsOffered($data) ? 'Marquer comme non offert': 'Marquer comme offert' }" ]
               --                                    []
               --                                , a [ attribute "data-bind" "click: function() { $root.editPresent($data); }" ]
               --                                    [ text "Modifier" ]
               --                                , a [ attribute "data-bind" "click: function() { $root.deletePresent($data); }" ]
               --                                    [ text "Supprimer" ]
               --                                , i [ class "reorder fa fa-bars", title "Glissez-déposez pour réordonner" ]
               --                                    []
               --                                ]
               --                            , h4 [ class "list-group-item-heading" ]
               --                                [ span [ class "title", attribute "data-bind" "text:title" ]
               --                                    [ text "Title" ]
               --                                , span [ class "offered-by", attribute "data-bind" "text: $root.displayPresentAsCreatedBy($data) || ''" ]
               --                                    []
               --                                , span [ class "offered-by", attribute "data-bind" "text: $root.displayPresentAsOffered($data) || ''" ]
               --                                    []
               --                                ]
               --                            , p [ class "list-group-item-text", attribute "data-bind" "markdown:description" ]
               --                                []
               --                            ]
               --                        ]
               --                    , button [ class "btn btn-primary", attribute "data-bind" "click: addPresent" ]
               --                        [ text "Ajouter un cadeau" ]
               --                    , div [ class "modal", hidden True, attribute "data-bind" "visible: editing", attribute "style" "display: block;" ]
               --                        [ div [ class "modal-dialog" ]
               --                            [ div [ class "modal-content" ]
               --                                [ div [ class "modal-header" ]
               --                                    [ button [ attribute "aria-hidden" "true", class "close", attribute "data-bind" "click: cancelEdition", attribute "data-dismiss" "modal", type_ "button" ]
               --                                        [ text "×" ]
               --                                    , h4 [ class "modal-title", attribute "data-bind" "text: editPopupText()" ]
               --                                        [ text "Popup title" ]
               --                                    ]
               --                                , div [ class "modal-body" ]
               --                                    [ form [ class "form", attribute "data-bind" "submit: saveEditedPresent" ]
               --                                        [ div [ class "form-group" ]
               --                                            [ label []
               --                                                [ text "Titre" ]
               --                                            , input [ class "form-control", attribute "data-bind" "value:$root.edition.title, hasfocus: editing()", type_ "text" ]
               --                                                []
               --                                            , text "                  "
               --                                            ]
               --                                        , div [ class "form-group" ]
               --                                            [ label []
               --                                                [ text "Description" ]
               --                                            , textarea [ class "form-control", attribute "data-bind" "markdownEditor:$root.edition.description, valueUpdate: 'afterkeydown'" ]
               --                                                []
               --                                            ]
               --                                        ]
               --                                    ]
               --                                , div [ class "modal-footer" ]
               --                                    [ button [ class "btn btn-default", attribute "data-bind" "click: cancelEdition", attribute "data-dismiss" "modal", type_ "button" ]
               --                                        [ text "Annuler" ]
               --                                    , button [ class "btn btn-primary", attribute "data-bind" "click: saveEditedPresent", type_ "button" ]
               --                                        [ text "Sauvegarder les changements" ]
               --                                    ]
               --                                ]
               --                            ]
               --                        ]
               --                    , div [ attribute "data-bind" "visible: slowShowingLoadingMessage() || successMessage() || undoAction()", id "command-execution" ]
               --                        [ div [ class "loading", attribute "data-bind" "text: slowShowingLoadingMessage" ]
               --                            []
               --                        , div [ class "confirm" ]
               --                            [ button [ class "close", attribute "data-bind" "visible: successMessage() || undoAction(), click: discardConfirm", type_ "button" ]
               --                                [ text "×" ]
               --                            , span [ attribute "data-bind" "text: successMessage" ]
               --                                []
               --                            , a [ attribute "data-bind" "visible: undoAction, click: undoAction()", href "" ]
               --                                [ text "Annuler" ]
               --                            ]
               --                        ]
               --                    ]
               , footer [] (textHtml "<footer>Développé avec amour par <a href='https://github.com/oadam/'>oadam</a><br>Icons by <a href='http://glyphicons.com/'>Glyphicons</a><br></footer>")
               ]
    }


viewPresent : Present -> Html Msg
viewPresent present =
    div
        [ class "mdl-card mdl-shadow--2dp" ]
        [ div [ class "mdl-card__title" ]
            [ h3 [ class "mdl-card__title-text" ] [ text present.title ] ]
        , div [ class "mdl-card__supporting-text" ] (textHtml present.body)
        ]


viewKeyedPresent : Present -> ( String, Html Msg )
viewKeyedPresent present =
    ( String.fromInt present.id, lazy viewPresent present )


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
