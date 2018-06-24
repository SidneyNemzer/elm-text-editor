module Editor.Update exposing (Msg(..), update)

import Array exposing (Array)
import Editor.Model exposing (InternalState, Position)


type Msg
    = CursorTo Position
    | CursorLeft
    | CursorRight
    | CursorUp
    | CursorDown


update : Array String -> Msg -> InternalState -> ( InternalState, Cmd Msg )
update lines msg state =
    case msg of
        CursorTo position ->
            ( { state | cursor = position }, Cmd.none )

        CursorLeft ->
            ( { state | cursor = movePositionColumn 1 lines state.cursor }
            , Cmd.none
            )

        CursorRight ->
            Debug.crash "TODO"

        CursorUp ->
            Debug.crash "TODO"

        CursorDown ->
            Debug.crash "TODO"


movePositionColumnLeft : Int -> Array String -> Position -> Position
movePositionColumnLeft int lines position =
    { position
        |
    }
