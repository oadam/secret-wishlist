module Pages.WishList exposing (Model, Msg, getSession, init, update, view)

import Api exposing (Present, PresentId, Token, User, UserId, userIdFromString, userIdToString)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, for, hidden, id, placeholder, required, selected, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import PendingModification exposing (PendingModification)
import Session exposing (Session)
import String.Interpolate exposing (interpolate)
import TextHtml exposing (textHtml)


type Msg
    = GotPresents (Result Http.Error (List Present))
    | ChangeList (Maybe UserId)
    | UpdatePresent Present
    | AddPresent
    | EditPresent Present
    | GotUpdatedPresent (Result Http.Error Present)
    | UpdateEditionTitle String
    | UpdateEditionDescription String
    | SubmitEdition


type State
    = Loading
    | Fail
    | ShowingPresents (List Present)
    | EditingPresent
        (List Present)
        { present : Maybe Present
        , title : String
        , description : String
        , hasChanges : Bool
        , saving : Bool
        }


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

        AddPresent ->
            case model.state of
                ShowingPresents presents ->
                    ( Model
                        { model
                            | state =
                                EditingPresent presents
                                    { present = Nothing
                                    , title = ""
                                    , description = ""
                                    , hasChanges = False
                                    , saving = False
                                    }
                        }
                    , Cmd.none
                    )

                _ ->
                    ( Model model, Cmd.none )

        EditPresent present ->
            case model.state of
                ShowingPresents presents ->
                    ( Model
                        { model
                            | state =
                                EditingPresent presents
                                    { present = Just present
                                    , title = present.title
                                    , description = present.description
                                    , hasChanges = False
                                    , saving = False
                                    }
                        }
                    , Cmd.none
                    )

                _ ->
                    ( Model model, Cmd.none )

        UpdatePresent present ->
            replacePresentIfLoaded True (Model model) present

        UpdateEditionTitle title ->
            case model.state of
                EditingPresent list edition ->
                    ( Model { model | state = EditingPresent list { edition | title = title, hasChanges = True } }, Cmd.none )

                _ ->
                    ( Model model, Cmd.none )

        UpdateEditionDescription desc ->
            case model.state of
                EditingPresent list edition ->
                    ( Model { model | state = EditingPresent list { edition | description = desc, hasChanges = True } }, Cmd.none )

                _ ->
                    ( Model model, Cmd.none )

        SubmitEdition ->
            case model.state of
                EditingPresent list edition ->
                    case edition.present of
                        Nothing ->
                            ( Model { model | state = EditingPresent list { edition | saving = True } }
                            , Api.addPresent model.session.token
                                { createdBy = model.session.logged_user.user_id
                                , description = edition.description
                                , title = edition.title
                                , to = model.user.user_id
                                }
                                GotUpdatedPresent
                            )

                        Just present ->
                            replacePresentIfLoaded True (Model model) { present | title = edition.title, description = edition.description }

                _ ->
                    ( Model model, Cmd.none )

        GotUpdatedPresent (Err _) ->
            ( Model model, Cmd.none )

        GotUpdatedPresent (Ok present) ->
            replacePresentIfLoaded False (Model model) present

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


replacePresent : Bool -> Model -> List Present -> Present -> ( Model, Cmd Msg )
replacePresent postToServer (Model model) presents present =
    ( Model
        { model
            | state =
                ShowingPresents
                    -- filter + prepend works even for new presents !
                    (present
                        :: List.filter
                            (\p -> p.id /= present.id)
                            presents
                    )
        }
    , if postToServer then
        Api.updatePresent model.session.token present GotUpdatedPresent

      else
        Cmd.none
    )


replacePresentIfLoaded : Bool -> Model -> Present -> ( Model, Cmd Msg )
replacePresentIfLoaded postToServer (Model model) present =
    case model.state of
        ShowingPresents presents ->
            replacePresent postToServer (Model model) presents present

        EditingPresent presents _ ->
            replacePresent postToServer (Model model) presents present

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

    in
    div [ class "card present", classList [ ( "offered", present.offeredBy /= Nothing ) ] ]
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


viewPresents : Model -> List Present -> List (Html Msg)
viewPresents (Model model) presents =
    let
        me = model.session.logged_user.user_id
        mine = me == model.user.user_id
        filteredPresents =
            presents
            |> List.filter (\p -> p.deletedBy == Nothing && (not mine || p.createdBy == me))
            |> List.map (\p -> {p| offeredBy = if mine && p.offeredBy /= Just me then Nothing else p.offeredBy})
        listHtml =
            if List.isEmpty filteredPresents
            then [ div [ class "alert alert-default" ] [ b [] [text "Cette liste est vide !"], br [] [], text "Vous pouvez ajouter un cadeau à l'aide du bouton ci-dessous." ]]
            else List.map (viewPresent (Model model))  filteredPresents
    in
        listHtml ++ [button [ id "add-present-fab", onClick AddPresent ] [ text "+" ]]


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
    let
        rest =
            case model.state of
                Loading ->
                    [ p [] [ text "chargement en cours..." ] ]

                Fail ->
                    [ div [ class "alert alert-danger" ] [ text "une erreur s'est produite sur le serveur" ] ]

                ShowingPresents presents ->
                    viewPresents (Model model) presents

                EditingPresent presents edition ->
                    [ form [ disabled (not edition.hasChanges || edition.saving || String.isEmpty edition.title || String.isEmpty edition.description), onSubmit SubmitEdition ]
                        [ div [ class "form-group" ]
                            [ label [ attribute "for" "present-title" ] [ text "Titre" ]
                            , input [ id "present-title", class "form-control", required True, value edition.title, onInput UpdateEditionTitle ] []
                            ]
                        , div [ class "form-group" ]
                            [ label [ attribute "for" "present-description" ] [ text "Description" ]
                            , textarea [ id "present-description", class "form-control", required True, value edition.description, onInput UpdateEditionDescription ] []
                            ]
                        , button [ class "btn btn-lg btn-primary btn-block", type_ "submit" ]
                            [ text "Sauvegarder" ]
                        ]
                    ]
    in
        nav [ attribute "aria-label" "breadcrumb" ]
            [ ol [ class "breadcrumb" ]
                (li [ class "breadcrumb-item" ] [ text "Secret Wishlist" ]
                    :: breadcrumbEnd (Model model)
                )
            ]
            :: rest
