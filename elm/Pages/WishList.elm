module Pages.WishList exposing (Model, Msg, init, update, view, getSession)

import Api exposing (Present, PresentId, Token, User, UserId, userIdFromString, userIdToString)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, for, hidden, id, placeholder, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import PendingModification exposing (PendingModification)
import Session exposing (Session)
import String.Interpolate exposing (interpolate)
import TextHtml exposing (textHtml)


type Msg
    = GotPresents (Result Http.Error (List Present))
    | ChangeList (Maybe UserId)
    | UpdatePresent Present
    | GotUpdatedPresent (Result Http.Error Present)


type Model msg
    = Model
        { session : Session
        , user : User
        , presents : Maybe (Result Http.Error (List Present))
        , wishListMsg : Msg -> msg
        , editPresentMessage : Present -> msg
        , pendingModifications : List PendingModification
        }

init : Session -> User -> List PendingModification -> (Msg -> msg) -> (Present -> msg) -> ( Model msg, Cmd msg )
init session user pendingModifications wishListMsg editPresentMessage =
    ( Model
        { session = session
        , user = user
        , presents = Nothing
        , wishListMsg = wishListMsg
        , editPresentMessage = editPresentMessage
        , pendingModifications = pendingModifications
        }
    , Api.getPresents session.token user.user_id (wishListMsg << GotPresents)
    )

getSession : Model msg -> Session
getSession (Model model) = model.session


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg (Model model) =
    case msg of
        GotPresents result ->
            ( Model { model | presents = Just result }
            , Cmd.none
            )

        UpdatePresent present ->
            ( Model { model | presents = replacePresent model.presents present }
            , Api.updatePresent model.session.token present (model.wishListMsg << GotUpdatedPresent)
            )

        GotUpdatedPresent (Err _) ->
            ( Model model, Cmd.none )

        GotUpdatedPresent (Ok present) ->
            ( Model { model | presents = replacePresent model.presents present }
            , Cmd.none
            )

        ChangeList Nothing ->
            ( Model model, Cmd.none )

        ChangeList (Just userId) ->
            let
                user =
                    List.head (List.filter (\u -> u.user_id == userId) model.session.users)
            in
            case user of
                Just u ->
                    init model.session u model.pendingModifications model.wishListMsg model.editPresentMessage

                Nothing ->
                    ( Model model, Cmd.none )


replacePresent : Maybe (Result Http.Error (List Present)) -> Present -> Maybe (Result Http.Error (List Present))
replacePresent presents present =
    Maybe.map
        (Result.map
            (List.map
                (\p ->
                    if p.id == present.id then
                        present

                    else
                        p
                )
            )
        )
        presents


viewPresent : Model msg -> Present -> Html msg
viewPresent (Model model) present =
    let
        offered =
            present.offeredBy /= Nothing

        deleted =
            present.deletedBy /= Nothing
    in
    div [ class "card present", hidden deleted, classList [ ( "offered", present.offeredBy /= Nothing ) ] ]
        [ div [ class "card-body" ]
            [ h5 [ class "card-title" ] [ text present.title ]
            , div [ class "card-text" ] (textHtml present.description)
            , div [ class "text-right" ]
                [ button [ class "btn card-link", hidden offered ] [ text "modifier" ]
                , button [ class "btn card-link", hidden offered, onClick <| model.wishListMsg <| UpdatePresent { present | offeredBy = Just model.session.logged_user.user_id } ] [ text "rayer" ]
                , button [ class "btn card-link", hidden (not offered), onClick <| model.wishListMsg <| UpdatePresent { present | offeredBy = Nothing } ] [ text "dé-rayer" ]
                , button [ class "btn card-link text-danger", onClick <| model.wishListMsg <| UpdatePresent { present | deletedBy = Just model.session.logged_user.user_id } ] [ text "supprimer" ]
                ]
            ]
        ]


viewPresents : Model msg -> List (Html msg)
viewPresents (Model model) =
    case model.presents of
        Nothing ->
            [ p [] [ text "chargement en cours..." ] ]

        Just (Err _) ->
            [ div [ class "alert alert-danger" ] [ text "une erreur s'est produite sur le serveur" ] ]

        Just (Ok presents) ->
            List.map (viewPresent (Model model)) presents


userOption : Model msg -> User -> Html msg
userOption (Model model) user =
    option [ selected (model.user.user_id == user.user_id), value (userIdToString user.user_id) ] [ text user.name ]


listSelect : Model msg -> Html msg
listSelect (Model model) =
    let
        options =
            List.map (userOption (Model model)) model.session.users
    in
    select [ id "list-picker", class "form-select", onInput (model.wishListMsg << ChangeList << userIdFromString) ] options


view : Model msg -> List (Html msg)
view (Model model) =
    nav [ attribute "aria-label" "breadcrumb" ]
        [ ol [ class "breadcrumb" ]
            [ li [ class "breadcrumb-item" ] [ text "Secret Wishlist" ]
            , li [ attribute "aria-current" "page", class "breadcrumb-item" ]
                [ listSelect (Model model)
                ]
            ]
        ]
        :: viewPresents (Model model)
