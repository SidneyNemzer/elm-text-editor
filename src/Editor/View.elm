module Editor.View exposing (view)

import Char
import Editor.Keymap
import Editor.Model exposing (InternalState)
import Editor.Update exposing (Msg(..))
import Html exposing (Attribute, Html, div, span, text)
import Html.Attributes as Attribute exposing (class, classList)
import Html.Events as Event
import Json.Decode as Decode
import Position exposing (Position)


name : String
name =
    "elm-editor"


selected : Position -> Maybe Position -> Position -> Bool
selected cursor maybeSelection char =
    maybeSelection
        |> Maybe.map (\selection -> Position.between cursor selection char)
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


withTrue : a -> ( a, Bool )
withTrue a =
    ( a, True )


captureOnMouseDown : Msg -> Attribute Msg
captureOnMouseDown msg =
    Event.stopPropagationOn
        "mousedown"
        (Decode.map withTrue (Decode.succeed msg))


captureOnMouseOver : Msg -> Attribute Msg
captureOnMouseOver msg =
    Event.stopPropagationOn
        "mouseover"
        (Decode.map withTrue (Decode.succeed msg))


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
        , captureOnMouseDown (MouseDown position)
        , captureOnMouseOver (MouseOver position)
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
        , captureOnMouseDown (MouseDown end)
        , captureOnMouseOver (MouseOver end)
        ]
        [ span
            [ class <| name ++ "-line__number"
            , captureOnMouseDown (MouseDown start)
            , captureOnMouseOver (MouseOver start)
            ]
            [ text <| String.fromInt (number + 1) ]
        , span
            [ class <| name ++ "-line__gutter-padding"
            , captureOnMouseDown (MouseDown start)
            , captureOnMouseOver (MouseOver start)
            ]
            [ text <| String.fromChar nbsp ]
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


onTripleClick : msg -> Attribute msg
onTripleClick msg =
    Event.on
        "click"
        (Decode.field "detail" Decode.int
            |> Decode.andThen
                (\detail ->
                    if detail >= 3 then
                        Decode.succeed msg

                    else
                        Decode.fail ""
                )
        )


view : List String -> InternalState -> Html Msg
view lines state =
    div
        [ class <| name ++ "-container"
        , Event.preventDefaultOn
            "keydown"
            (Decode.map withTrue Editor.Keymap.decoder)
        , Event.onMouseUp MouseUp
        , Event.onDoubleClick SelectGroup
        , onTripleClick SelectLine
        , Attribute.tabindex 0
        ]
    <|
        List.indexedMap (line state.cursor state.selection) lines
