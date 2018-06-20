module Editor.Update exposing (Msg(..), update)

import Editor.Model exposing (InternalState)


type Msg
    = NoOp


update : Msg -> InternalState -> ( InternalState, Cmd Msg )
update msg state =
    case msg of
        NoOp ->
            ( state, Cmd.none )
