module Editor.View exposing (view)

import Char
import Html exposing (Html, Attribute, span, div, text)
import Html.Attributes as Attribute exposing (class, classList)
import Html.Events as Event
import Editor.Model exposing (Position, InternalState)
import Editor.Update exposing (Msg(..))


name : String
name =
    "elm-editor"


between : Int -> Int -> Int -> Bool
between start end point =
    if start > end then
        between end start point
    else if start == end then
        start == point
    else
        point >= start && point <= end


selected : Position -> Maybe Position -> Position -> Bool
selected cursor maybeSelection { line, column } =
    Maybe.map
        (\selection ->
            if cursor.line == selection.line then
                line
                    == cursor.line
                    && between cursor.column selection.column column
            else if cursor.line == line then
                between 1 cursor.column column
            else if selection.line == line then
                between 1 selection.column column
            else
                between cursor.line selection.line line
        )
        maybeSelection
        |> Maybe.withDefault False


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
                , ( name ++ "-line__character--selected"
                  , selected cursor selection position
                  )
                ]
            , Event.onMouseDown (MoveCursorTo position)
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


view : List String -> InternalState -> Html Msg
view lines state =
    div [ class <| name ++ "-container" ] <|
        List.indexedMap (line state.cursor state.selection) lines
