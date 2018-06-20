module Editor.View exposing (view)

import Array.Hamt as Array exposing (Array)
import Char
import Html exposing (Html, Attribute, text, div, span)
import Html.Attributes as Attribute exposing (class)
import Html.Events as Events
import Json.Decode as Decode
import Editor.Types exposing (Position, Msg(..), Hover(..), InternalState, Selection(..))
import Editor.Keymap exposing (keyDecoder)


name : String
name =
    "elm-editor"


captureClick : msg -> Attribute msg
captureClick msg =
    Events.onWithOptions
        "click"
        { stopPropagation = True, preventDefault = True }
        (Decode.succeed msg)


nbsp : Char
nbsp =
    Char.fromCode 160


spaceToNbsp : String -> String
spaceToNbsp =
    String.map
        (\char ->
            if char == ' ' then
                nbsp
            else
                char
        )


captureHover : msg -> Attribute msg
captureHover msg =
    Events.onWithOptions
        "mouseover"
        { stopPropagation = True, preventDefault = True }
        (Decode.succeed msg)


character : Bool -> Bool -> msg -> String -> Html msg
character selected hasCursor onHover content =
    let
        ( cursor, cursorClass ) =
            if hasCursor then
                ( span [ class <| name ++ "-cursor" ] [ text " " ]
                , "--has-cursor"
                )
            else
                ( text "", "" )
    in
        span
            [ class <| name ++ "-line__character" ++ cursorClass
            , captureHover onHover
            ]
            [ text <| spaceToNbsp content, cursor ]


line : Position -> Int -> String -> Html Msg
line cursor number content =
    let
        onHoverChar column =
            Hover <| HoverChar <| { line = number, column = column }

        positionedCharacter column =
            character
                False
                (cursor.line == number && cursor.column == column)
                (onHoverChar column)
    in
        div
            [ class <| name ++ "-line"
            , captureHover (HoverLine number |> Hover)
            ]
        <|
            [ span
                [ class <| name ++ "-line__number"
                , captureHover <| onHoverChar 0
                ]
                [ text <| toString number ]
            , span [ class <| name ++ "-line__content" ]
                (content
                    |> String.split ""
                    |> List.indexedMap positionedCharacter
                )
            ]
                ++ if
                    cursor.line
                        == number
                        && cursor.column
                        >= String.length content
                   then
                    [ span [ class <| name ++ "-line__character--has-cursor" ]
                        [ text " "
                        , Html.span [ class <| name ++ "-cursor" ] [ text " " ]
                        ]
                    ]
                   else
                    []


view : InternalState -> Html Msg
view state =
    div
        [ class <| name ++ "-container"
        , Events.on "keydown" keyDecoder
        , Events.onMouseDown StartSelecting
        , Events.onMouseUp StopSelecting
        , Events.onMouseOut (Hover NoHover)
        , Attribute.tabindex 0
        ]
        (state.lines
            |> Array.toList
            |> List.indexedMap (line state.cursor)
        )
