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
    | EditPresent Present
    | GotUpdatedPresent (Result Http.Error Present)
    | UpdateEditionTitle String
    | UpdateEditionDescription String


type State
    = Loading
    | Fail
    | ShowingPresents (List Present)
    | EditingPresent (List Present) { id : Maybe PresentId, title : String, description : String }


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
            ( Model { model | state = ShowingPresents presents }
            , Cmd.none
            )

        EditPresent present ->
            case model.state of
                ShowingPresents presents ->
                    ( Model { model | state = EditingPresent presents { id = Just present.id, title = present.title, description = present.description } }
                    , Cmd.none
                    )

                _ ->
                    ( Model model, Cmd.none )

        UpdatePresent present ->
            replacePresentIfLoaded (Model model) present

        UpdateEditionTitle title ->
            case model.state of
                EditingPresent list edition ->
                    ( Model { model | state = EditingPresent list { edition | title = title } }, Cmd.none )

                _ ->
                    ( Model model, Cmd.none )

        UpdateEditionDescription desc ->
            case model.state of
                EditingPresent list edition ->
                    ( Model { model | state = EditingPresent list { edition | description = desc } }, Cmd.none )

                _ ->
                    ( Model model, Cmd.none )

        GotUpdatedPresent (Err _) ->
            ( Model model, Cmd.none )

        GotUpdatedPresent (Ok present) ->
            replacePresentIfLoaded (Model model) present

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


replacePresent : Model -> List Present -> Present -> ( Model, Cmd Msg )
replacePresent (Model model) presents present =
    ( Model
        { model
            | state =
                ShowingPresents
                    (List.map
                        (\p ->
                            if p.id == present.id then
                                present

                            else
                                p
                        )
                        presents
                    )
        }
    , Api.updatePresent model.session.token present GotUpdatedPresent
    )


replacePresentIfLoaded : Model -> Present -> ( Model, Cmd Msg )
replacePresentIfLoaded (Model model) present =
    case model.state of
        ShowingPresents presents ->
            replacePresent (Model model) presents present

        EditingPresent presents _ ->
            replacePresent (Model model) presents present

        _ ->
            ( Model model, Cmd.none )


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
                    , onClick <| EditPresent present
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

        ShowingPresents presents ->
            List.map (viewPresent (Model model)) presents

        EditingPresent presents edition ->
            [ form []
                [ div [ class "form-group" ]
                    [ label [ attribute "for" "present-title" ] [ text "Titre" ]
                    , input [ id "present-title", class "form-control", value edition.title, onInput UpdateEditionTitle ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ attribute "for" "present-description" ] [ text "Description" ]
                    , textarea [ id "present-description", class "form-control", onInput UpdateEditionDescription ] [ text edition.description ]
                    ]
                ]
            ]


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


breadcrumbEditionTitle : String -> String
breadcrumbEditionTitle title =
    if String.isEmpty title then
        "Nouveau cadeau"

    else
        title


breadcrumbEnd : Model -> List (Html Msg)
breadcrumbEnd (Model model) =
    case model.state of
        EditingPresent _ edition ->
            [ li [ class "breadcrumb-item" ]
                [ a [ attribute "role" "button" ]
                    [ text model.user.name ]
                ]
            , li [ attribute "aria-current" "page", class "breadcrumb-item" ]
                [ text (breadcrumbEditionTitle edition.title)
                ]
            ]

        _ ->
            [ li [ attribute "aria-current" "page", class "breadcrumb-item" ]
                [ listSelect (Model model)
                ]
            ]


view : Model -> List (Html Msg)
view (Model model) =
    nav [ attribute "aria-label" "breadcrumb" ]
        [ ol [ class "breadcrumb" ]
            (li [ class "breadcrumb-item" ] [ text "Secret Wishlist" ]
                :: breadcrumbEnd (Model model)
            )
        ]
        :: viewPresents (Model model)
