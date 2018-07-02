module Editor.Update exposing (Msg(..), update)

import Array exposing (Array)
import Position exposing (Position)
import Editor.Model exposing (InternalState)
import Buffer.Basic as Buffer exposing (Buffer)


type Msg
    = MouseDown Position
    | MouseOver Position
    | MouseUp
    | CursorLeft
    | CursorRight
    | CursorUp
    | CursorDown
    | InsertChar Char
    | RemoveCharAfter
    | RemoveCharBefore


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
                        Position.previousColumn state.cursor
                            |> clampPosition False lines
                  }
                , buffer
                , Cmd.none
                )

            CursorRight ->
                ( { state
                    | cursor =
                        Position.nextColumn state.cursor
                            |> clampPosition True lines
                  }
                , buffer
                , Cmd.none
                )

            CursorUp ->
                ( { state
                    | cursor =
                        Position.previousLine state.cursor
                            |> clampPosition False lines
                  }
                , buffer
                , Cmd.none
                )

            CursorDown ->
                ( { state
                    | cursor =
                        Position.nextLine state.cursor
                            |> clampPosition False lines
                  }
                , buffer
                , Cmd.none
                )

            InsertChar char ->
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
                            , Buffer.replace
                                start
                                end
                                (String.fromChar char)
                                buffer
                            , Cmd.none
                            )

                    Nothing ->
                        ( { state
                            | cursor = Position.nextColumn state.cursor
                          }
                        , Buffer.insert state.cursor (String.fromChar char) buffer
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
