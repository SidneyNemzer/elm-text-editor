module Editor.Update exposing (Msg(..), update)

import Dict exposing (Dict)
import Position exposing (Position)
import Editor.Model exposing (InternalState)
import Buffer exposing (Buffer)


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
                            |> Buffer.clampPosition Buffer.Backward buffer
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
                            |> Buffer.clampPosition Buffer.Forward buffer
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
                            |> Buffer.clampPosition Buffer.Backward buffer
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
                            |> Buffer.clampPosition Buffer.Backward buffer
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
                        case Buffer.lineEnd moveFrom.line buffer of
                            Just column ->
                                Position.setColumn column state.cursor

                            Nothing ->
                                Buffer.clampPosition
                                    Buffer.Backward
                                    buffer
                                    state.cursor
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
            case ( state.selection, Dict.get string autoclose ) of
                ( Just selection, Just closing ) ->
                    let
                        ( start, end ) =
                            Position.order selection state.cursor

                        wrapped =
                            string
                                ++ Buffer.between start end buffer
                                ++ closing
                    in
                        ( { state
                            | cursor =
                                if state.cursor.line == start.line then
                                    Position.nextColumn state.cursor
                                else
                                    state.cursor
                            , selection =
                                Just <|
                                    if selection.line == start.line then
                                        Position.nextColumn selection
                                    else
                                        selection
                          }
                        , Buffer.replace start end wrapped buffer
                        , Cmd.none
                        )

                ( Just selection, Nothing ) ->
                    let
                        ( start, end ) =
                            Position.order selection state.cursor
                    in
                        ( { state
                            | cursor =
                                if string == "\n" then
                                    { line = start.line + 1
                                    , column = 0
                                    }
                                else
                                    Position.nextColumn start
                            , selection = Nothing
                          }
                        , Buffer.replace start end string buffer
                        , Cmd.none
                        )

                ( Nothing, maybeClosing ) ->
                    let
                        nearWordChar =
                            Buffer.nearWordChar state.cursor buffer

                        insertString =
                            if not nearWordChar then
                                Maybe.map ((++) string) maybeClosing
                                    |> Maybe.withDefault string
                            else
                                string
                    in
                        ( { state
                            | cursor =
                                if string == "\n" then
                                    { line = state.cursor.line + 1, column = 0 }
                                else
                                    Position.nextColumn state.cursor
                          }
                        , Buffer.insert state.cursor insertString buffer
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
                                |> Buffer.clampPosition Buffer.Backward buffer
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
                    , Buffer.indentBetween state.cursor selection buffer
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
                        ( deindentedBuffer, cursorColumn, selectionColumn ) =
                            Buffer.deindentBetween state.cursor selection buffer
                    in
                        ( { state
                            | cursor =
                                Position.setColumn cursorColumn state.cursor
                            , selection =
                                Just <|
                                    Position.setColumn selectionColumn selection
                          }
                        , deindentedBuffer
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
                        |> Buffer.clampPosition Buffer.Backward buffer
            in
                ( { state
                    | cursor = cursor
                    , selection =
                        if state.selection == Just cursor then
                            Nothing
                        else if
                            (state.selection == Nothing)
                                && (state.cursor /= cursor)
                        then
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
                        |> Buffer.clampPosition Buffer.Backward buffer
            in
                ( { state
                    | cursor = cursor
                    , selection =
                        if state.selection == Just cursor then
                            Nothing
                        else if
                            (state.selection == Nothing)
                                && (state.cursor /= cursor)
                        then
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
                        |> Buffer.clampPosition Buffer.Backward buffer
            in
                ( { state
                    | cursor = cursor
                    , selection =
                        if state.selection == Just cursor then
                            Nothing
                        else if
                            (state.selection == Nothing)
                                && (state.cursor /= cursor)
                        then
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
                        |> Buffer.clampPosition Buffer.Forward buffer
            in
                ( { state
                    | cursor = cursor
                    , selection =
                        if state.selection == Just cursor then
                            Nothing
                        else if
                            (state.selection == Nothing)
                                && (state.cursor /= cursor)
                        then
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
                        if state.selection == Just cursor then
                            Nothing
                        else if
                            (state.selection == Nothing)
                                && (state.cursor /= cursor)
                        then
                            Just state.cursor
                        else
                            state.selection
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
                        if state.selection == Just cursor then
                            Nothing
                        else if
                            (state.selection == Nothing)
                                && (state.cursor /= cursor)
                        then
                            Just state.cursor
                        else
                            state.selection
                  }
                , buffer
                , Cmd.none
                )

        SelectToGroupStart ->
            let
                cursor =
                    Buffer.groupStart state.cursor buffer
            in
                ( { state
                    | cursor = cursor
                    , selection =
                        if state.selection == Just cursor then
                            Nothing
                        else if
                            (state.selection == Nothing)
                                && (state.cursor /= cursor)
                        then
                            Just state.cursor
                        else
                            state.selection
                  }
                , buffer
                , Cmd.none
                )

        SelectToGroupEnd ->
            let
                cursor =
                    Buffer.groupEnd state.cursor buffer
            in
                ( { state
                    | cursor = cursor
                    , selection =
                        if state.selection == Just cursor then
                            Nothing
                        else if
                            (state.selection == Nothing)
                                && (state.cursor /= cursor)
                        then
                            Just state.cursor
                        else
                            state.selection
                  }
                , buffer
                , Cmd.none
                )

        SelectAll ->
            ( { state
                | cursor = Buffer.end buffer
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
