module Main exposing (main)

import Api exposing (Present, PresentId, Token, User, UserId)
import Browser
import Help
import Html exposing (Html, a, br, button, div, footer, h1, h3, header, main_, nav, p, span, text, u)
import Html.Attributes exposing (attribute, class, classList, hidden, href)
import Html.Events exposing (onClick)
import Pages.Login as Login exposing (Msg)
import Pages.WishList as WishList
import Session exposing (Session)
import String.Interpolate exposing (interpolate)


type Page
    = Login Login.Model
    | WishList WishList.Model


type alias Model =
    { page : Page, help : Bool }


type Msg
    = ToggleHelp
    | LoginMsg Login.Msg
    | WishListMsg WishList.Msg
    | EditPresentMsg Present
    | Logout


init : () -> ( Model, Cmd Msg )
init flags =
    ( { page =
            Login <| Login.init
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
                newSession =
                    Login.isMsgStartSession msg

                ( loginModel, cmd ) =
                    Login.update msg login
            in
            case newSession of
                Nothing ->
                    ( { model | page = Login loginModel }, Cmd.map LoginMsg cmd )

                Just session ->
                    let
                        ( wiModel, wiCmd ) =
                            WishList.init session session.logged_user []
                    in
                    ( { model | page = WishList wiModel }
                    , Cmd.map WishListMsg wiCmd
                    )

        ( WishListMsg msg, WishList wishList ) ->
            let
                ( wishListModel, cmd ) =
                    WishList.update msg wishList
            in
            ( { model | page = WishList wishListModel }, Cmd.map WishListMsg cmd )

        ( Logout, _ ) ->
            init ()

        _ ->
            ( model, Cmd.none )


windowTitle : Page -> String
windowTitle page =
    case page of
        _ ->
            "Secret Wishlist"


viewMain : Page -> List (Html Msg)
viewMain page =
    case page of
        Login login ->
            List.map (Html.map LoginMsg) (Login.view login)
                ++ [ p [ class "lead text-center" ]
                        [ button [ class "btn btn-link", onClick ToggleHelp ]
                            [ text "Comment ça marche ?" ]
                        ]
                   ]

        WishList wishList ->
            List.map (Html.map WishListMsg) (WishList.view wishList)


viewLogout : Page -> List (Html Msg)
viewLogout page =
    let
        session =
            getSession page
    in
    case session of
        Nothing ->
            []

        Just s ->
            [ nav [ class "nav nav-masthead justify-content-center" ]
                [ span [] [ span [ class "text-muted" ] [ text "connecté en tant que " ], span [] [ text s.logged_user.name ] ]
                , button [ class "btn", onClick Logout ]
                    [ text "Déconnexion" ]
                ]
            ]


centerMain : Page -> Bool
centerMain page =
    case page of
        Login _ ->
            True

        _ ->
            False


getSession : Page -> Maybe Session
getSession page =
    case page of
        Login _ ->
            Nothing

        WishList wishlist ->
            Just <| WishList.getSession wishlist


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
                    [ header [ class "masthead", classList [ ( "mb-auto", centerMain page ) ] ]
                        [ div [ class "inner" ]
                            (h3 [ class "masthead-brand" ]
                                [ text "Secret Wishlist" ]
                                :: viewLogout page
                            )
                        ]
                    , main_ [ class "inner", classList [ ( "cover", centerMain page ) ], attribute "role" "main" ] (viewMain page)
                    , footer [ class "mastfoot mt-auto" ]
                        [ div [ class "inner" ]
                            [ p []
                                [ text "Créé par Olivier Adam. Code source sur "
                                , a [ href "https://www.github.com/oadam/secret-wishlist" ]
                                    [ text "Github" ]
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
