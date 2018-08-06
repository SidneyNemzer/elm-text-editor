module Editor.Update exposing (Msg(..), update)

import Dict exposing (Dict)
import Array.Hamt as Array exposing (Array)
import Position exposing (Position)
import Editor.Model exposing (InternalState)
import Buffer exposing (Buffer)
import Util.Array


type Msg
    = MouseDown Position
    | MouseOver Position
    | MouseUp
    | CursorLeft
    | CursorRight
    | CursorUp
    | CursorDown
    | CursorToLineEnd
    | CursorToLineStart
    | CursorToGroupEnd
    | CursorToGroupStart
    | Insert String
    | RemoveCharAfter
    | RemoveCharBefore
    | RemoveGroupAfter
    | RemoveGroupBefore
    | Indent
    | Deindent
    | SelectUp
    | SelectDown
    | SelectLeft
    | SelectRight
    | SelectToLineStart
    | SelectToLineEnd
    | SelectToGroupStart
    | SelectToGroupEnd
    | SelectAll
    | SelectGroup
    | SelectLine


autoclose : Dict String String
autoclose =
    Dict.fromList
        [ ( "[", "]" )
        , ( "{", "}" )
        , ( "(", ")" )
        , ( "\"", "\"" )
        , ( "'", "'" )
        , ( "`", "`" )
        ]


update : Buffer -> Msg -> InternalState -> ( InternalState, Buffer, Cmd Msg )
update buffer msg state =
    let
        lines =
            buffer |> Buffer.lines |> Array.fromList
    in
        case msg of
            MouseDown position ->
                ( { state
                    | cursor = position
                    , dragging = True
                    , selection = Nothing
                  }
                , buffer
                , Cmd.none
                )

            MouseOver position ->
                if state.dragging then
                    ( { state
                        | selection =
                            case state.selection of
                                Just selection ->
                                    if selection == position then
                                        Nothing
                                    else
                                        Just selection

                                Nothing ->
                                    if position == state.cursor then
                                        Nothing
                                    else
                                        Just state.cursor
                        , cursor = position
                      }
                    , buffer
                    , Cmd.none
                    )
                else
                    ( state, buffer, Cmd.none )

            MouseUp ->
                ( { state | dragging = False }, buffer, Cmd.none )

            CursorLeft ->
                ( { state
                    | cursor =
                        let
                            moveFrom =
                                case state.selection of
                                    Just selection ->
                                        Position.order selection state.cursor
                                            |> Tuple.first

                                    Nothing ->
                                        state.cursor
                        in
                            Position.previousColumn moveFrom
                                |> clampPosition False lines
                    , selection = Nothing
                  }
                , buffer
                , Cmd.none
                )

            CursorRight ->
                ( { state
                    | cursor =
                        let
                            moveFrom =
                                case state.selection of
                                    Just selection ->
                                        Position.order selection state.cursor
                                            |> Tuple.second

                                    Nothing ->
                                        state.cursor
                        in
                            Position.nextColumn moveFrom
                                |> clampPosition True lines
                    , selection = Nothing
                  }
                , buffer
                , Cmd.none
                )

            CursorUp ->
                ( { state
                    | cursor =
                        let
                            moveFrom =
                                case state.selection of
                                    Just selection ->
                                        Position.order selection state.cursor
                                            |> Tuple.first

                                    Nothing ->
                                        state.cursor
                        in
                            Position.previousLine moveFrom
                                |> clampPosition False lines
                    , selection = Nothing
                  }
                , buffer
                , Cmd.none
                )

            CursorDown ->
                ( { state
                    | cursor =
                        let
                            moveFrom =
                                case state.selection of
                                    Just selection ->
                                        Position.order selection state.cursor
                                            |> Tuple.second

                                    Nothing ->
                                        state.cursor
                        in
                            Position.nextLine moveFrom
                                |> clampPosition False lines
                    , selection = Nothing
                  }
                , buffer
                , Cmd.none
                )

            CursorToLineEnd ->
                ( { state
                    | cursor =
                        let
                            moveFrom =
                                case state.selection of
                                    Just selection ->
                                        Position.order selection state.cursor
                                            |> Tuple.second

                                    Nothing ->
                                        state.cursor
                        in
                            case Array.get moveFrom.line lines of
                                Just line ->
                                    Position.setColumn
                                        (String.length line)
                                        state.cursor

                                Nothing ->
                                    clampPosition False lines state.cursor
                    , selection = Nothing
                  }
                , buffer
                , Cmd.none
                )

            CursorToLineStart ->
                ( { state
                    | cursor =
                        let
                            moveFrom =
                                case state.selection of
                                    Just selection ->
                                        Position.order selection state.cursor
                                            |> Tuple.first

                                    Nothing ->
                                        state.cursor
                        in
                            Position.setColumn 0 moveFrom
                    , selection = Nothing
                  }
                , buffer
                , Cmd.none
                )

            CursorToGroupEnd ->
                ( { state
                    | cursor = Buffer.groupEnd state.cursor buffer
                    , selection = Nothing
                  }
                , buffer
                , Cmd.none
                )

            CursorToGroupStart ->
                ( { state
                    | cursor = Buffer.groupStart state.cursor buffer
                    , selection = Nothing
                  }
                , buffer
                , Cmd.none
                )

            Insert string ->
                case state.selection of
                    Just selection ->
                        let
                            ( start, end ) =
                                Position.order selection state.cursor
                        in
                            case Dict.get string autoclose of
                                Just closing ->
                                    ( { state
                                        | cursor =
                                            Position.nextColumn state.cursor
                                        , selection =
                                            Just <|
                                                if selection.line == start.line then
                                                    Position.nextColumn selection
                                                else
                                                    selection
                                      }
                                    , Buffer.insert start string Dict.empty buffer
                                        |> Buffer.insert
                                            (Position.nextColumn end)
                                            closing
                                            Dict.empty
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( { state
                                        | cursor =
                                            if string == "\n" then
                                                { line = start.line + 1
                                                , column = 0
                                                }
                                            else
                                                start
                                        , selection = Nothing
                                      }
                                    , Buffer.replace start end string buffer
                                    , Cmd.none
                                    )

                    Nothing ->
                        ( { state
                            | cursor =
                                if string == "\n" then
                                    { line = state.cursor.line + 1, column = 0 }
                                else
                                    Position.nextColumn state.cursor
                          }
                        , Buffer.insert state.cursor string autoclose buffer
                        , Cmd.none
                        )

            RemoveCharAfter ->
                case state.selection of
                    Just selection ->
                        let
                            ( start, end ) =
                                Position.order selection state.cursor
                        in
                            ( { state
                                | cursor = start
                                , selection = Nothing
                              }
                            , Buffer.replace start end "" buffer
                            , Cmd.none
                            )

                    Nothing ->
                        ( state
                        , Buffer.replace
                            state.cursor
                            (Position.nextColumn state.cursor)
                            ""
                            buffer
                        , Cmd.none
                        )

            RemoveCharBefore ->
                case state.selection of
                    Just selection ->
                        let
                            ( start, end ) =
                                Position.order selection state.cursor
                        in
                            ( { state
                                | cursor = start
                                , selection = Nothing
                              }
                            , Buffer.replace start end "" buffer
                            , Cmd.none
                            )

                    Nothing ->
                        ( { state
                            | cursor =
                                Position.previousColumn state.cursor
                                    -- use old buffer to place cursor at the
                                    -- end of the old line
                                    |> clampPosition False lines
                          }
                        , Buffer.removeBefore state.cursor buffer
                        , Cmd.none
                        )

            RemoveGroupAfter ->
                case state.selection of
                    Just selection ->
                        let
                            ( start, end ) =
                                Position.order selection state.cursor
                        in
                            ( { state
                                | cursor = start
                                , selection = Nothing
                              }
                            , Buffer.replace start end "" buffer
                            , Cmd.none
                            )

                    Nothing ->
                        let
                            end =
                                Buffer.groupEnd state.cursor buffer
                        in
                            ( state
                            , Buffer.replace state.cursor end "" buffer
                            , Cmd.none
                            )

            RemoveGroupBefore ->
                case state.selection of
                    Just selection ->
                        let
                            ( start, end ) =
                                Position.order selection state.cursor
                        in
                            ( { state
                                | cursor = start
                                , selection = Nothing
                              }
                            , Buffer.replace start end "" buffer
                            , Cmd.none
                            )

                    Nothing ->
                        let
                            start =
                                Buffer.groupStart state.cursor buffer
                        in
                            ( { state | cursor = start }
                            , Buffer.replace start state.cursor "" buffer
                            , Cmd.none
                            )

            Indent ->
                case state.selection of
                    Just selection ->
                        let
                            ( start, end ) =
                                Position.order selection state.cursor
                        in
                            ( { state
                                | cursor =
                                    Position.addColumn
                                        Buffer.indentSize
                                        state.cursor
                                , selection =
                                    Just <|
                                        Position.addColumn
                                            Buffer.indentSize
                                            selection
                              }
                            , List.range start.line end.line
                                |> List.foldl
                                    (\line buffer ->
                                        Buffer.indentFrom
                                            (Position line 0)
                                            buffer
                                            |> Tuple.first
                                    )
                                    buffer
                            , Cmd.none
                            )

                    Nothing ->
                        let
                            ( indentedBuffer, indentedColumn ) =
                                Buffer.indentFrom state.cursor buffer
                        in
                            ( { state
                                | cursor =
                                    Position.setColumn indentedColumn state.cursor
                              }
                            , indentedBuffer
                            , Cmd.none
                            )

            Deindent ->
                case state.selection of
                    Just selection ->
                        let
                            ( start, end ) =
                                Position.order selection state.cursor
                        in
                            ( { state
                                | cursor =
                                    Position.addColumn
                                        -Buffer.indentSize
                                        state.cursor
                                , selection =
                                    Just <|
                                        Position.addColumn
                                            -Buffer.indentSize
                                            selection
                              }
                            , List.range (start.line + 1) (end.line - 1)
                                |> List.foldl
                                    (\line buffer ->
                                        Buffer.deindentFrom
                                            (Position line 0)
                                            buffer
                                            |> Tuple.first
                                    )
                                    buffer
                            , Cmd.none
                            )

                    Nothing ->
                        let
                            ( deindentedBuffer, deindentedColumn ) =
                                Buffer.deindentFrom state.cursor buffer
                        in
                            ( { state
                                | cursor =
                                    Position.setColumn deindentedColumn state.cursor
                              }
                            , deindentedBuffer
                            , Cmd.none
                            )

            SelectUp ->
                let
                    cursor =
                        Position.previousLine state.cursor
                            |> clampPosition False lines
                in
                    ( { state
                        | cursor = cursor
                        , selection =
                            if
                                (state.selection == Just cursor)
                                    || (state.cursor == cursor)
                            then
                                Nothing
                            else if state.selection == Nothing then
                                Just state.cursor
                            else
                                state.selection
                      }
                    , buffer
                    , Cmd.none
                    )

            SelectDown ->
                let
                    cursor =
                        Position.nextLine state.cursor
                            |> clampPosition False lines
                in
                    ( { state
                        | cursor = cursor
                        , selection =
                            if
                                (state.selection == Just cursor)
                                    || (state.cursor == cursor)
                            then
                                Nothing
                            else if state.selection == Nothing then
                                Just state.cursor
                            else
                                state.selection
                      }
                    , buffer
                    , Cmd.none
                    )

            SelectLeft ->
                let
                    cursor =
                        Position.previousColumn state.cursor
                            |> clampPosition False lines
                in
                    ( { state
                        | cursor = cursor
                        , selection =
                            if
                                (state.selection == Just cursor)
                                    || (state.cursor == cursor)
                            then
                                Nothing
                            else if state.selection == Nothing then
                                Just state.cursor
                            else
                                state.selection
                      }
                    , buffer
                    , Cmd.none
                    )

            SelectRight ->
                let
                    cursor =
                        Position.nextColumn state.cursor
                            |> clampPosition True lines
                in
                    ( { state
                        | cursor = cursor
                        , selection =
                            if
                                (state.selection == Just cursor)
                                    || (state.cursor == cursor)
                            then
                                Nothing
                            else if state.selection == Nothing then
                                Just state.cursor
                            else
                                state.selection
                      }
                    , buffer
                    , Cmd.none
                    )

            SelectToLineStart ->
                let
                    cursor =
                        Position.setColumn 0 state.cursor
                in
                    ( { state
                        | cursor = cursor
                        , selection =
                            state.selection
                                |> Maybe.withDefault state.cursor
                                |> Just
                                |> Maybe.andThen
                                    (\selection ->
                                        if selection == cursor then
                                            Nothing
                                        else
                                            Just selection
                                    )
                      }
                    , buffer
                    , Cmd.none
                    )

            SelectToLineEnd ->
                let
                    cursor =
                        Position.setColumn
                            (Buffer.lineEnd state.cursor.line buffer
                                |> Maybe.withDefault state.cursor.line
                            )
                            state.cursor
                in
                    ( { state
                        | cursor = cursor
                        , selection =
                            state.selection
                                |> Maybe.withDefault state.cursor
                                |> Just
                                |> Maybe.andThen
                                    (\selection ->
                                        if selection == cursor then
                                            Nothing
                                        else
                                            Just selection
                                    )
                      }
                    , buffer
                    , Cmd.none
                    )

            SelectToGroupStart ->
                case state.selection of
                    Just selection ->
                        let
                            ( start, end ) =
                                Position.order selection state.cursor
                        in
                            ( { state
                                | cursor = Buffer.groupStart start buffer
                                , selection =
                                    if start == end then
                                        Nothing
                                    else
                                        Just start
                              }
                            , buffer
                            , Cmd.none
                            )

                    Nothing ->
                        let
                            cursor =
                                Buffer.groupStart state.cursor buffer
                        in
                            ( { state
                                | cursor = cursor
                                , selection =
                                    if state.cursor == cursor then
                                        Nothing
                                    else
                                        Just state.cursor
                              }
                            , buffer
                            , Cmd.none
                            )

            SelectToGroupEnd ->
                case state.selection of
                    Just selection ->
                        let
                            ( start, end ) =
                                Position.order selection state.cursor
                        in
                            ( { state
                                | cursor = Buffer.groupEnd end buffer
                                , selection =
                                    if start == end then
                                        Nothing
                                    else
                                        Just end
                              }
                            , buffer
                            , Cmd.none
                            )

                    Nothing ->
                        let
                            cursor =
                                Buffer.groupEnd state.cursor buffer
                        in
                            ( { state
                                | cursor = cursor
                                , selection =
                                    if state.cursor == cursor then
                                        Nothing
                                    else
                                        Just state.cursor
                              }
                            , buffer
                            , Cmd.none
                            )

            SelectAll ->
                ( { state
                    | cursor =
                        case Util.Array.last lines of
                            Just ( line, index ) ->
                                Position index (String.length line)

                            Nothing ->
                                -- impossible, there is always at least one line
                                state.cursor
                    , selection = Just (Position 0 0)
                  }
                , buffer
                , Cmd.none
                )

            SelectGroup ->
                let
                    range =
                        Buffer.groupRange state.cursor buffer
                in
                    case range of
                        Just ( start, end ) ->
                            ( { state | cursor = end, selection = Just start }
                            , buffer
                            , Cmd.none
                            )

                        Nothing ->
                            ( state, buffer, Cmd.none )

            SelectLine ->
                ( { state
                    | cursor =
                        Buffer.lineEnd state.cursor.line buffer
                            |> Maybe.map
                                (\column ->
                                    Position.setColumn column state.cursor
                                )
                            |> Maybe.withDefault state.cursor
                    , selection = Just <| Position.setColumn 0 state.cursor
                  }
                , buffer
                , Cmd.none
                )


{-| Make sure the cursor is at a valid positon in the buffer.

If the cursor is:

    - before the first line, move to the first column of the first line
    - after the last line, move to the last column of the last line
    - past the last column of the line, if we should perfer the next line, go
      to the first column of the next line. Otherwise, go to the last column
      of the current line.
    - before the first column of the line, go to the last column of the previous
      line.

The first argument, `perferNextLine`, is used if we want to move to the right
in the buffer.

-}
clampPosition : Bool -> Array String -> Position -> Position
clampPosition preferNextLine lines position =
    if position.line < 0 then
        Position 0 0
    else
        case Array.get position.line lines of
            Just line ->
                if position.column > String.length line then
                    if preferNextLine then
                        Position (position.line + 1) 0
                            |> clampPosition True lines
                    else
                        Position position.line (String.length line)
                else if position.column < 0 then
                    Array.get (position.line - 1) lines
                        |> Maybe.map
                            (String.length >> Position (position.line - 1))
                        |> Maybe.withDefault (Position 0 0)
                else
                    position

            Nothing ->
                case Util.Array.last lines of
                    Just ( line, number ) ->
                        Position number (String.length line)

                    Nothing ->
                        Position 0 0
