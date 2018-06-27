module Editor exposing (State, Msg, init, update, view)

import Array
import Html exposing (Html)
import Editor.Model exposing (InternalState, Position)
import Editor.Update
import Editor.View


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


update : List String -> Msg -> State -> ( State, Cmd Msg )
update lines msg (State state) =
    Editor.Update.update (Array.fromList lines) msg state
        |> Tuple.mapFirst State


view : List String -> State -> Html Msg
view lines (State state) =
    Editor.View.view lines state
