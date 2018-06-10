module Editor.View exposing (container, line)

import Html exposing (Html, Attribute, text)
import Html.Attributes as Attribute exposing (class)


name : String
name =
    "elm-editor"


container : List (Attribute msg) -> List (Html msg) -> Html msg
container attrs =
    Html.div ([ class <| name ++ "-container" ])


line : ( Int, Int ) -> Int -> String -> Html msg
line ( cursorX, cursorY ) number content =
    Html.div [ class <| name ++ "-line" ] <|
        [ Html.span [ class <| name ++ "-line__number" ]
            [ text <| toString number ]
        ]
            ++ if cursorY == number then
                [ Html.span [] [ text <| String.slice 0 cursorX content ]
                , Html.span [ class <| name ++ "-line__content--has-cursor" ]
                    [ text <| String.slice cursorX (cursorX + 1) content
                    , Html.span [ class <| name ++ "-cursor" ] [ text " " ]
                    ]
                , Html.span []
                    [ text <|
                        String.slice
                            (cursorX + 1)
                            (String.length content)
                            content
                    ]
                ]
               else
                [ Html.span [ class <| name ++ "-line__content" ]
                    [ text content ]
                ]
