module Help exposing (modal)

import Html exposing (..)
import Html.Attributes exposing (attribute, class, hidden, id, type_)
import Html.Events exposing (onClick)
import TextHtml exposing (textHtml)


modal : msg -> List (Html msg)
modal closeMsg =
    [ div [ class "modal show" ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h4 [ class "modal-title" ]
                        [ text "Bienvenue sur dedguenodgo 2.0 !" ]
                    , button [ attribute "aria-hidden" "true", class "close", onClick closeMsg, type_ "button" ]
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
                    [ button [ class "btn btn-primary", onClick closeMsg, type_ "button" ]
                        [ text "J'ai compris !" ]
                    ]
                ]
            ]
        ]
    , div [ class "modal-backdrop show" ] []
    ]
