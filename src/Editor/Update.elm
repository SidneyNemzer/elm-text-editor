module Editor.Update exposing (Msg(..), update)

import Array exposing (Array)
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
                                Just position ->
                                    Just position

                                Nothing ->
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
                            ( { state
                                | cursor =
                                    if string == "\n" then
                                        { line = start.line + 1, column = 0 }
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
                        , Buffer.insert state.cursor string buffer
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
                ( { state
                    | cursor =
                        Position.previousLine state.cursor
                            |> clampPosition False lines
                    , selection =
                        if state.selection == Nothing then
                            Just state.cursor
                        else
                            state.selection
                  }
                , buffer
                , Cmd.none
                )

            SelectDown ->
                ( { state
                    | cursor =
                        Position.nextLine state.cursor
                            |> clampPosition False lines
                    , selection =
                        if state.selection == Nothing then
                            Just state.cursor
                        else
                            state.selection
                  }
                , buffer
                , Cmd.none
                )

            SelectLeft ->
                ( { state
                    | cursor =
                        Position.previousColumn state.cursor
                            |> clampPosition False lines
                    , selection =
                        if state.selection == Nothing then
                            Just state.cursor
                        else
                            state.selection
                  }
                , buffer
                , Cmd.none
                )

            SelectRight ->
                ( { state
                    | cursor =
                        Position.nextColumn state.cursor
                            |> clampPosition True lines
                    , selection =
                        if state.selection == Nothing then
                            Just state.cursor
                        else
                            state.selection
                  }
                , buffer
                , Cmd.none
                )

            SelectToLineStart ->
                ( { state
                    | cursor = Position.setColumn 0 state.cursor
                    , selection =
                        state.selection
                            |> Maybe.withDefault state.cursor
                            |> Just
                  }
                , buffer
                , Cmd.none
                )

            SelectToLineEnd ->
                ( { state
                    | cursor =
                        case Array.get state.cursor.line lines of
                            Just line ->
                                Position.setColumn
                                    (String.length line)
                                    state.cursor

                            Nothing ->
                                clampPosition False lines state.cursor
                    , selection =
                        state.selection
                            |> Maybe.withDefault state.cursor
                            |> Just
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
                                , selection = Just start
                              }
                            , buffer
                            , Cmd.none
                            )

                    Nothing ->
                        ( { state
                            | cursor = Buffer.groupStart state.cursor buffer
                            , selection = Just state.cursor
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
                                , selection = Just end
                              }
                            , buffer
                            , Cmd.none
                            )

                    Nothing ->
                        ( { state
                            | cursor = Buffer.groupEnd state.cursor buffer
                            , selection = Just state.cursor
                          }
                        , buffer
                        , Cmd.none
                        )

            SelectAll ->
                ( { state
                    | cursor =
                        case arrayLast lines of
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


arrayLast : Array a -> Maybe ( a, Int )
arrayLast array =
    let
        length =
            Array.length array
    in
        Array.slice -1 length array
            |> Array.get 0
            |> Maybe.map (\a -> ( a, length - 1 ))


clampPosition : Bool -> Array String -> Position -> Position
clampPosition preferNextLine lines position =
    -- line is less than first line -> first column first line
    -- line doesn't exist -> last column last line
    -- column is greater than line ->
    -- perferNextLine ? first column next line : last column of line
    -- column is less than 0 -> last column of previous line
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
                case arrayLast lines of
                    Just ( line, number ) ->
                        Position number (String.length line)

                    Nothing ->
                        Position 0 0
