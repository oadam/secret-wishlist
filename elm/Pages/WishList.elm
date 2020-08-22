module Pages.WishList exposing (Model, Msg, getSession, init, update, view)

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


type State
    = Loading
    | Fail
    | ShowPresents (List Present)


type Model
    = Model
        { session : Session
        , user : User
        , state : State
        , pendingModifications : List PendingModification
        }


init : Session -> User -> List PendingModification -> ( Model, Cmd Msg )
init session user pendingModifications =
    ( Model
        { session = session
        , user = user
        , state = Loading
        , pendingModifications = pendingModifications
        }
    , Api.getPresents session.token user.user_id GotPresents
    )


getSession : Model -> Session
getSession (Model model) =
    model.session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        GotPresents (Err _) ->
            ( Model { model | state = Fail }
            , Cmd.none
            )

        GotPresents (Ok presents) ->
            ( Model { model | state = ShowPresents presents }
            , Cmd.none
            )

        UpdatePresent present ->
            ( Model { model | state = replacePresent model.state present }
            , Api.updatePresent model.session.token present GotUpdatedPresent
            )

        GotUpdatedPresent (Err _) ->
            ( Model model, Cmd.none )

        GotUpdatedPresent (Ok present) ->
            ( Model { model | state = replacePresent model.state present }
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
                    init model.session u model.pendingModifications

                Nothing ->
                    ( Model model, Cmd.none )


replacePresent : State -> Present -> State
replacePresent state present =
    case state of
        ShowPresents presents ->
            ShowPresents
                (List.map
                    (\p ->
                        if p.id == present.id then
                            present

                        else
                            p
                    )
                    presents
                )

        _ ->
            state


fontawesome : String -> Html Msg
fontawesome icon =
    i [ class ("fa fa-" ++ icon), attribute "aria-hidden" "true" ] []


viewPresent : Model -> Present -> Html Msg
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
            , div [ class "present-buttons" ]
                [ button
                    [ class "btn btn-default"
                    , hidden offered
                    ]
                    [ fontawesome "pencil", text " modifier" ]
                , button
                    [ class "btn"
                    , hidden offered
                    , onClick <| UpdatePresent { present | offeredBy = Just model.session.logged_user.user_id }
                    ]
                    [ fontawesome "check-square-o", text " rayer" ]
                , button
                    [ class "btn"
                    , hidden (not offered)
                    , onClick <| UpdatePresent { present | offeredBy = Nothing }
                    ]
                    [ fontawesome "square-o", text " dé-rayer" ]
                , button
                    [ class "btn text-danger"
                    , onClick <| UpdatePresent { present | deletedBy = Just model.session.logged_user.user_id }
                    ]
                    [ fontawesome "trash", text " supprimer" ]
                ]
            ]
        ]


viewPresents : Model -> List (Html Msg)
viewPresents (Model model) =
    case model.state of
        Loading ->
            [ p [] [ text "chargement en cours..." ] ]

        Fail ->
            [ div [ class "alert alert-danger" ] [ text "une erreur s'est produite sur le serveur" ] ]

        ShowPresents presents ->
            List.map (viewPresent (Model model)) presents


userOption : Model -> User -> Html Msg
userOption (Model model) user =
    option [ selected (model.user.user_id == user.user_id), value (userIdToString user.user_id) ] [ text user.name ]


listSelect : Model -> Html Msg
listSelect (Model model) =
    let
        options =
            List.map (userOption (Model model)) model.session.users
    in
    select [ id "list-picker", class "form-select", onInput (ChangeList << userIdFromString) ] options


view : Model -> List (Html Msg)
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
