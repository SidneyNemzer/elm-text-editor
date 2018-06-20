module Editor.View exposing (view)

import Html exposing (Html, Attribute, span, div, text)
import Html.Attributes as Attribute exposing (class)
import Editor.Model exposing (Position, InternalState)


name : String
name =
    "elm-editor"


line : Position -> Int -> String -> Html msg
line cursor number content =
    div [ class <| name ++ "-line" ] <|
        [ span [ class <| name ++ "-line__number" ]
            [ text <| toString number ]
        ]
            ++ if cursor.line == number then
                [ span [] [ text <| String.slice 0 cursor.column content ]
                , span [ class <| name ++ "-line__content--has-cursor" ]
                    [ text <| String.slice cursor.column (cursor.column + 1) content
                    , span [ class <| name ++ "-cursor" ] [ text " " ]
                    ]
                , span []
                    [ text <|
                        String.slice
                            (cursor.column + 1)
                            (String.length content)
                            content
                    ]
                ]
               else
                [ span [ class <| name ++ "-line__content" ]
                    [ text content ]
                ]


view : List String -> InternalState -> Html msg
view lines state =
    div [ class <| name ++ "-container" ] <|
        List.indexedMap (line state.cursor) lines
