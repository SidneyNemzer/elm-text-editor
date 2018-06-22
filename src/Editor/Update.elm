module Editor.Update exposing (Msg(..), update)

import Editor.Model exposing (InternalState, Position)


type Msg
    = NoOp
    | MoveCursorTo Position


update : Msg -> InternalState -> ( InternalState, Cmd Msg )
update msg state =
    case msg of
        NoOp ->
            ( state, Cmd.none )

        MoveCursorTo position ->
            ( { state | cursor = position }, Cmd.none )
