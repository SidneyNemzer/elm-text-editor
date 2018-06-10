module Editor exposing (State, Msg, init, update, view)

import Array.Hamt as Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Editor.View as View


type Hover
    = NoHover
    | HoverLine Int
    | HoverChar Position


type Selection
    = NoSelection
    | SelectingFrom Hover
    | SelectedChar Position
    | Selection Position Position


type alias Position =
    { line : Int
    , column : Int
    }


type Msg
    = NoOp
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | NewLine
    | InsertChar Char
    | RemoveCharBefore
    | RemoveCharAfter
    | Hover Hover
    | GoToHoveredPosition
    | StartSelecting
    | StopSelecting


type alias InternalState =
    { lines : Array String
    , cursor : Position
    , hover : Hover
    , selection : Selection
    }


type State
    = State InternalState


init : String -> State
init content =
    State
        { lines = Array.fromList <| String.split "\n" content
        , cursor = Position 0 0
        , hover = NoHover
        , selection = NoSelection
        }


keyDecoder : Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToMsg


keyToMsg : String -> Decoder Msg
keyToMsg string =
    case String.uncons string of
        Just ( char, "" ) ->
            Decode.succeed (InsertChar char)

        _ ->
            case string of
                "ArrowUp" ->
                    Decode.succeed MoveUp

                "ArrowDown" ->
                    Decode.succeed MoveDown

                "ArrowLeft" ->
                    Decode.succeed MoveLeft

                "ArrowRight" ->
                    Decode.succeed MoveRight

                "Backspace" ->
                    Decode.succeed RemoveCharBefore

                "Delete" ->
                    Decode.succeed RemoveCharAfter

                "Enter" ->
                    Decode.succeed NewLine

                _ ->
                    Decode.fail "This key does nothing"


update : Msg -> State -> ( State, Cmd Msg )
update msg (State state) =
    let
        ( newState, cmd ) =
            case msg of
                NoOp ->
                    ( state, Cmd.none )

                MoveUp ->
                    ( { state | cursor = moveUp state.cursor state.lines }
                    , Cmd.none
                    )

                MoveDown ->
                    ( { state | cursor = moveDown state.cursor state.lines }
                    , Cmd.none
                    )

                MoveLeft ->
                    ( { state | cursor = moveLeft state.cursor state.lines }
                    , Cmd.none
                    )

                MoveRight ->
                    ( { state | cursor = moveRight state.cursor state.lines }
                    , Cmd.none
                    )

                NewLine ->
                    ( newLine state
                        |> sanitizeHover
                    , Cmd.none
                    )

                InsertChar char ->
                    ( insertChar char state
                    , Cmd.none
                    )

                RemoveCharBefore ->
                    ( removeCharBefore state
                        |> sanitizeHover
                    , Cmd.none
                    )

                RemoveCharAfter ->
                    ( removeCharAfter state
                        |> sanitizeHover
                    , Cmd.none
                    )

                Hover hover ->
                    ( { state | hover = hover }
                        |> sanitizeHover
                    , Cmd.none
                    )

                GoToHoveredPosition ->
                    ( { state
                        | cursor =
                            case state.hover of
                                NoHover ->
                                    state.cursor

                                HoverLine line ->
                                    { line = line
                                    , column = lastColumn state.lines line
                                    }

                                HoverChar position ->
                                    position
                      }
                    , Cmd.none
                    )

                StartSelecting ->
                    ( { state | selection = SelectingFrom state.hover }
                    , Cmd.none
                    )

                StopSelecting ->
                    -- Selection for all other
                    let
                        endHover =
                            state.hover

                        newSelection =
                            case state.selection of
                                NoSelection ->
                                    NoSelection

                                SelectingFrom startHover ->
                                    if startHover == endHover then
                                        case startHover of
                                            NoHover ->
                                                NoSelection

                                            HoverLine _ ->
                                                NoSelection

                                            HoverChar position ->
                                                SelectedChar position
                                    else
                                        hoversToPositions state.lines startHover endHover
                                            |> Maybe.map (\( from, to ) -> Selection from to)
                                            |> Maybe.withDefault NoSelection

                                SelectedChar _ ->
                                    NoSelection

                                Selection _ _ ->
                                    NoSelection
                    in
                        ( { state | selection = newSelection }
                        , Cmd.none
                        )
    in
        ( State newState, cmd )


hoversToPositions : Array String -> Hover -> Hover -> Maybe ( Position, Position )
hoversToPositions lines from to =
    let
        selectionLinePosition : Int -> Position -> ( Position, Position )
        selectionLinePosition line position =
            if line >= position.line then
                ( position
                , { line = line, column = lastColumn lines line }
                )
            else
                ( { line = line + 1, column = 0 }
                , position
                )
    in
        case ( from, to ) of
            ( NoHover, _ ) ->
                Nothing

            ( _, NoHover ) ->
                Nothing

            ( HoverLine line1, HoverLine line2 ) ->
                let
                    smaller =
                        min line1 line2

                    bigger =
                        max line1 line2
                in
                    Just
                        ( { line = smaller + 1, column = 0 }
                        , { line = bigger, column = lastColumn lines bigger }
                        )

            ( HoverLine line, HoverChar position ) ->
                Just (selectionLinePosition line position)

            ( HoverChar position, HoverLine line ) ->
                Just (selectionLinePosition line position)

            ( HoverChar position1, HoverChar position2 ) ->
                let
                    ( smaller, bigger ) =
                        if comparePositions position1 position2 == LT then
                            ( position1, position2 )
                        else
                            ( position2, position1 )
                in
                    Just ( smaller, bigger )


comparePositions : Position -> Position -> Order
comparePositions from to =
    if from.line < to.line || (from.line == to.line && from.column < to.column) then
        LT
    else if from == to then
        EQ
    else
        GT


sanitizeHover : InternalState -> InternalState
sanitizeHover state =
    { state
        | hover =
            case state.hover of
                NoHover ->
                    state.hover

                HoverLine line ->
                    HoverLine (clamp 0 (lastLine state.lines) line)

                HoverChar { line, column } ->
                    let
                        sanitizedLine =
                            clamp 0 (lastLine state.lines) line

                        sanitizedColumn =
                            clamp 0 (lastColumn state.lines sanitizedLine) column
                    in
                        HoverChar
                            { line = sanitizedLine
                            , column = sanitizedColumn
                            }
    }


newLine : InternalState -> InternalState
newLine ({ cursor, lines } as state) =
    let
        { line, column } =
            cursor

        linesList : List String
        linesList =
            Array.toList lines

        line_ : Int
        line_ =
            line + 1

        contentUntilCursor : List String
        contentUntilCursor =
            linesList
                |> List.take line_
                |> List.indexedMap
                    (\i content ->
                        if i == line then
                            String.left column content
                        else
                            content
                    )

        restOfLineAfterCursor : String
        restOfLineAfterCursor =
            String.dropLeft column (lineContent lines line)

        restOfLines : List String
        restOfLines =
            List.drop line_ linesList

        newLines : Array String
        newLines =
            (contentUntilCursor
                ++ [ restOfLineAfterCursor ]
                ++ restOfLines
            )
                |> Array.fromList

        newCursor : Position
        newCursor =
            { line = line_
            , column = 0
            }
    in
        { state
            | lines = newLines
            , cursor = newCursor
        }


insertChar : Char -> InternalState -> InternalState
insertChar char ({ cursor, lines } as state) =
    let
        { line, column } =
            cursor

        lineWithCharAdded : String -> String
        lineWithCharAdded content =
            String.left column content
                ++ String.fromChar char
                ++ String.dropLeft column content

        newLines : Array String
        newLines =
            lines
                |> Array.indexedMap
                    (\i content ->
                        if i == line then
                            lineWithCharAdded content
                        else
                            content
                    )

        newCursor : Position
        newCursor =
            { line = line
            , column = column + 1
            }
    in
        { state
            | lines = newLines
            , cursor = newCursor
        }


removeCharBefore : InternalState -> InternalState
removeCharBefore ({ cursor, lines } as state) =
    if isStartOfDocument cursor then
        state
    else
        let
            { line, column } =
                cursor

            lineIsEmpty : Bool
            lineIsEmpty =
                lineContent lines line
                    |> String.isEmpty

            removeCharFromLine : ( Int, String ) -> List String
            removeCharFromLine ( lineNum, content ) =
                if lineNum == line - 1 then
                    if isFirstColumn column then
                        [ content ++ lineContent lines line ]
                    else
                        [ content ]
                else if lineNum == line then
                    if isFirstColumn column then
                        []
                    else
                        [ String.left (column - 1) content
                            ++ String.dropLeft column content
                        ]
                else
                    [ content ]

            newLines : Array String
            newLines =
                lines
                    |> Array.toIndexedList
                    |> List.concatMap removeCharFromLine
                    |> Array.fromList
        in
            { state
                | lines = newLines
                , cursor = moveLeft cursor lines
            }


removeCharAfter : InternalState -> InternalState
removeCharAfter ({ cursor, lines } as state) =
    if isEndOfDocument lines cursor then
        state
    else
        let
            { line, column } =
                cursor

            isOnLastColumn : Bool
            isOnLastColumn =
                isLastColumn lines line column

            removeCharFromLine : ( Int, String ) -> List String
            removeCharFromLine ( lineNum, content ) =
                if lineNum == line then
                    if isOnLastColumn then
                        [ content ++ lineContent lines (line + 1) ]
                    else
                        [ String.left column content
                            ++ String.dropLeft (column + 1) content
                        ]
                else if lineNum == line + 1 then
                    if isOnLastColumn then
                        []
                    else
                        [ content ]
                else
                    [ content ]

            newLines : Array String
            newLines =
                lines
                    |> Array.toIndexedList
                    |> List.concatMap removeCharFromLine
                    |> Array.fromList
        in
            { state
                | lines = newLines
                , cursor = cursor
            }


moveUp : Position -> Array String -> Position
moveUp { line, column } lines =
    if isFirstLine line then
        startOfDocument
    else
        let
            line_ : Int
            line_ =
                previousLine line
        in
            { line = line_
            , column = clampColumn lines line_ column
            }


moveDown : Position -> Array String -> Position
moveDown { line, column } lines =
    if isLastLine lines line then
        endOfDocument lines
    else
        let
            line_ : Int
            line_ =
                nextLine lines line
        in
            { line = line_
            , column = clampColumn lines line_ column
            }


moveLeft : Position -> Array String -> Position
moveLeft ({ line, column } as position) lines =
    if isStartOfDocument position then
        position
    else if isFirstColumn column then
        let
            line_ : Int
            line_ =
                previousLine line
        in
            { line = line_
            , column = lastColumn lines line_
            }
    else
        { line = line
        , column = column - 1
        }


moveRight : Position -> Array String -> Position
moveRight ({ line, column } as position) lines =
    if isEndOfDocument lines position then
        position
    else if isLastColumn lines line column then
        { line = nextLine lines line
        , column = 0
        }
    else
        { line = line
        , column = column + 1
        }


startOfDocument : Position
startOfDocument =
    { line = 0
    , column = 0
    }


endOfDocument : Array String -> Position
endOfDocument lines =
    { line = lastLine lines
    , column = lastColumn lines (lastLine lines)
    }


isStartOfDocument : Position -> Bool
isStartOfDocument { line, column } =
    isFirstLine line
        && isFirstColumn column


isEndOfDocument : Array String -> Position -> Bool
isEndOfDocument lines { line, column } =
    isLastLine lines line
        && isLastColumn lines line column


isFirstLine : Int -> Bool
isFirstLine line =
    line == 0


isLastLine : Array String -> Int -> Bool
isLastLine lines line =
    line == lastLine lines


isFirstColumn : Int -> Bool
isFirstColumn column =
    column == 0


isLastColumn : Array String -> Int -> Int -> Bool
isLastColumn lines line column =
    column == lastColumn lines line


lastLine : Array String -> Int
lastLine lines =
    Array.length lines - 1


previousLine : Int -> Int
previousLine line =
    (line - 1)
        |> max 0


nextLine : Array String -> Int -> Int
nextLine lines line =
    (line + 1)
        |> min (maxLine lines)


maxLine : Array String -> Int
maxLine lines =
    Array.length lines - 1


lastColumn : Array String -> Int -> Int
lastColumn lines line =
    lineLength lines line


clampColumn : Array String -> Int -> Int -> Int
clampColumn lines line column =
    column
        |> clamp 0 (lineLength lines line)


lineContent : Array String -> Int -> String
lineContent lines lineNum =
    lines
        |> Array.get lineNum
        |> Maybe.withDefault ""


lineLength : Array String -> Int -> Int
lineLength lines lineNum =
    lineContent lines lineNum
        |> String.length


view : State -> Html Msg
view (State state) =
    View.container
        [ Events.on "keydown" keyDecoder
        , Attribute.tabindex 0
        ]
        (state.lines
            |> Array.toList
            |> List.indexedMap (View.line state.cursor)
        )
