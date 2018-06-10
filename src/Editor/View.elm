module Editor.View exposing (container, line)

import Html exposing (Html, Attribute, text)
import Html.Attributes as Attribute exposing (class)


type alias Position =
    { line : Int
    , column : Int
    }


name : String
name =
    "elm-editor"


container : List (Attribute msg) -> List (Html msg) -> Html msg
container attrs =
    Html.div ([ class <| name ++ "-container" ] ++ attrs)


line : Position -> Int -> String -> Html msg
line position number rawContent =
        Html.div [ class <| name ++ "-line" ] <|
            [ Html.span [ class <| name ++ "-line__number" ]
                [ text <| toString number ]
            ]
                ++ if position.line == number then
                    [ Html.span [] [ text <| String.slice 0 position.column content ]
                    , Html.span [ class <| name ++ "-line__content--has-cursor" ]
                        [ text <| String.slice position.column (position.column + 1) content
                        , Html.span [ class <| name ++ "-cursor" ] [ text " " ]
                        ]
                    , Html.span []
                        [ text <|
                            String.slice
                                (position.column + 1)
                                (String.length content)
                                content
                        ]
                    ]
                   else
                    [ Html.span [ class <| name ++ "-line__content" ]
                        [ text content ]
                    ]
