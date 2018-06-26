module Editor.View exposing (view)

import Char
import Html exposing (Html, Attribute, span, div, text)
import Html.Attributes as Attribute exposing (class, classList)
import Html.Events as Event
import Json.Decode as Decode
import Editor.Model exposing (Position, InternalState)
import Editor.Update exposing (Msg(..))
import Editor.Keymap


name : String
name =
    "elm-editor"


between : Int -> Int -> Int -> Bool
between start end point =
    if start > end then
        between end start point
    else
        (start /= end)
            && (point >= start)
            && (point < end)


betweenPositions : Position -> Position -> Position -> Bool
betweenPositions start end ({ line, column } as position) =
    if start.line > end.line then
        betweenPositions end start position
    else if start.line == end.line then
        (line == start.line)
            && between start.column end.column column
    else if start.line == line then
        column >= start.column
    else if end.line == line then
        column < end.column
    else
        between start.line end.line line


selected : Position -> Maybe Position -> Position -> Bool
selected cursor maybeSelection character =
    maybeSelection
        |> Maybe.map (\selection -> betweenPositions cursor selection character)
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


captureOnMouseDown : Msg -> Attribute Msg
captureOnMouseDown msg =
    Event.onWithOptions
        "mousedown"
        { preventDefault = False
        , stopPropagation = True
        }
        (Decode.succeed msg)


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
            , captureOnMouseDown (CursorTo position)
            ]
            [ text <| String.fromChar <| ensureNbsp char
            , if hasCursor then
                span [ class <| name ++ "-cursor" ] [ text " " ]
              else
                text ""
            ]


line : Position -> Maybe Position -> Int -> String -> Html Msg
line cursor selection number content =
    let
        end =
            Position number (String.length content)

        start =
            Position number 0
    in
        div
            [ class <| name ++ "-line"
            , captureOnMouseDown (CursorTo end)
            ]
            [ span
                [ class <| name ++ "-line__number"
                , captureOnMouseDown (CursorTo start)
                ]
                [ text <| toString number ]
            , span
                [ class <| name ++ "-line__gutter-padding"
                , captureOnMouseDown (CursorTo start)
                ]
                [ text " " ]
            , span [ class <| name ++ "-line__content" ]
                (content
                    |> String.toList
                    |> List.indexedMap
                        (character cursor selection << Position number)
                )
            , if
                (cursor.line == number)
                    && (cursor.column >= String.length content)
              then
                span
                    [ class <| name ++ "-line__character"
                    , class <| name ++ "-line__character--has-cursor"
                    ]
                    [ text " "
                    , span [ class <| name ++ "-cursor" ] [ text " " ]
                    ]
              else
                text ""
            ]


view : List String -> InternalState -> Html Msg
view lines state =
    div
        [ class <| name ++ "-container"
        , Event.on "keydown" Editor.Keymap.decoder
        , Attribute.tabindex 0
        ]
    <|
        List.indexedMap (line state.cursor state.selection) lines
