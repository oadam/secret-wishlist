module Popups exposing (..)

import Html exposing (..)
import Html.Attributes exposing (attribute, class, hidden, id, type_)
import Html.Events exposing (onClick)
import TextHtml exposing (textHtml)


type Msg
    = HideWelcome


welcome : Bool -> List (Html Msg)
welcome visible =
    [ div [ hidden (not visible), class "modal" ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h4 [ class "modal-title" ]
                        [ text "Bienvenue sur dedguenodgo 2.0 !" ]
                    , button [ attribute "aria-hidden" "true", class "close", onClick HideWelcome, type_ "button" ]
                        [ text "×" ]
                    ]
                , div [ class "modal-body" ]
                    (textHtml """
                            <p>Ce site permet, à la manière d'un petit carnet en papier, de réaliser des listes de Noël. Vous pouvez ajouter des cadeaux et les rayer si vous comptez les offrir.
                            <br>
                            Ce qui est nouveau c'est que dans votre propre liste on ne vous montre pas tout, le but étant de ne pas vous gacher la surprise.
                            </p>
                            <h4>Exemples</h4>
                            <ul>
                                    <li>Anne ajoute "nain de jardin" dans sa liste. Le lendemain Franck raye le cadeau pour dire qu'il va l'acheter. Tout le monde sauf Anne voit que le cadeau est rayé"</li>
                                    <li>Olivier trouve la liste d'Elisabeth trop vide donc il y ajoute "vernis à ongles mauve". Tout le monde sauf Elisabeth voit le cadeau dans la liste. Anne achète le cadeau et le raye. Tout le monde voit le cadeau rayé. Elisabeth ne voit toujours pas le cadeau.</li>
                            </ul>""")
                , div [ class "modal-footer" ]
                    [ button [ class "btn btn-primary", onClick HideWelcome, type_ "button" ]
                        [ text "J'ai compris !" ]
                    ]
                ]
            ]
        ]
    , div [ class "modal-backdrop show", hidden (not visible) ] []
    ]


loading : Bool -> Html msg
loading visible =
    div [ hidden (not visible) ]
        [ div [ id "loading-message" ]
            [ text "Le chargement peut durer 30 secondes..." ]
        ]


error : Bool -> Html msg
error visible =
    div [ id "error", hidden (not visible) ]
        [ text "ERROR" ]
