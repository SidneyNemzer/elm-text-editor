module Editor.View exposing (view)

import Char
import Html exposing (Html, Attribute, span, div, text)
import Html.Attributes as Attribute exposing (class, classList)
import Editor.Model exposing (Position, InternalState)
import Editor.Update exposing (Msg(..))


name : String
name =
    "elm-editor"


nbsp : Char
nbsp =
    Char.fromCode 160


ensureNbsp : Char -> Char
ensureNbsp char =
    if char == ' ' then
        nbsp
    else
        char


character : Position -> Maybe Position -> Position -> Char -> Html Msg
character cursor selection position char =
    let
        hasCursor =
            cursor == position
    in
        span
            [ classList
                [ ( name ++ "-line__character", True )
                , ( name ++ "-line__character--has-cursor", hasCursor )
                ]
            ]
            [ text <| String.fromChar <| ensureNbsp char
            , if hasCursor then
                span [ class <| name ++ "-cursor" ] [ text " " ]
              else
                text ""
            ]


line : Position -> Maybe Position -> Int -> String -> Html Msg
line cursor selection number content =
    div [ class <| name ++ "-line" ]
        [ span [ class <| name ++ "-line__number" ]
            [ text <| toString number ]
        , span [ class <| name ++ "-line__content" ]
            (content
                |> String.toList
                |> List.indexedMap
                    (character cursor selection << Position number)
            )
        ]


view : List String -> InternalState -> Html msg
view lines state =
    div [ class <| name ++ "-container" ] <|
        List.indexedMap (line state.cursor state.selection) lines
