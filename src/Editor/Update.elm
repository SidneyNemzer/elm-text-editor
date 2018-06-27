module Editor.Update exposing (Msg(..), update)

import Array exposing (Array)
import Editor.Model exposing (InternalState, Position)


type Msg
    = MouseDown Position
    | CursorLeft
    | CursorRight
    | CursorUp
    | CursorDown


update : Array String -> Msg -> InternalState -> ( InternalState, Cmd Msg )
update lines msg state =
    case msg of
            ( { state | cursor = position }, Cmd.none )
        MouseDown position ->

        CursorLeft ->
            ( { state | cursor = movePositionLeft 1 lines state.cursor }
            , Cmd.none
            )

        CursorRight ->
            ( { state | cursor = movePositionRight 1 lines state.cursor }
            , Cmd.none
            )

        CursorUp ->
            ( { state | cursor = movePositionUp 1 lines state.cursor }
            , Cmd.none
            )

        CursorDown ->
            ( { state | cursor = movePositionDown 1 lines state.cursor }
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


clampPosition : Array String -> Position -> Position
clampPosition lines position =
    -- line is less than first line -> first column first line
    -- line doesn't exist -> last column last line
    -- column is greater than line -> first column of next line
    -- column is less than beginning -> last column of previous line
    if position.line < 0 then
        Position 0 0
    else
        case Array.get position.line lines of
            Just line ->
                if position.column > String.length line then
                    Position (position.line + 1) 0
                        |> clampPosition lines
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


movePositionLeft : Int -> Array String -> Position -> Position
movePositionLeft distance lines position =
    { position | column = position.column - distance }
        |> clampPosition lines


movePositionRight : Int -> Array String -> Position -> Position
movePositionRight distance lines position =
    { position | column = position.column + distance }
        |> clampPosition lines


movePositionUp : Int -> Array String -> Position -> Position
movePositionUp distance lines position =
    { position | line = position.line - distance }
        |> clampPosition lines


movePositionDown : Int -> Array String -> Position -> Position
movePositionDown distance lines position =
    { position | line = position.line + distance }
        |> clampPosition lines
