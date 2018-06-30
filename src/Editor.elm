module Editor exposing (State, Msg, init, update, view)

import Array
import Html exposing (Html)
import Position exposing (Position)
import Editor.Model exposing (InternalState)
import Editor.Update
import Editor.View
import Buffer.Basic as Buffer exposing (Buffer)


type alias Msg =
    Editor.Update.Msg


type State
    = State InternalState


init : State
init =
    State
        { scrolledLine = 0
        , cursor = Position 0 0
        , selection = Nothing
        , dragging = False
        }


update : Buffer -> Msg -> State -> ( State, Buffer, Cmd Msg )
update buffer msg (State state) =
    Editor.Update.update buffer msg state
        |> (\( state, buffer, cmd ) -> ( State state, buffer, cmd ))


view : Buffer -> State -> Html Msg
view buffer (State state) =
    Editor.View.view (Buffer.lines buffer) state
