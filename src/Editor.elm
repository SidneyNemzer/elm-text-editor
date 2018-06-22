module Editor exposing (State, Msg, init, update, view)

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
        }


view : List String -> State -> Html msg
update : Msg -> State -> ( State, Cmd Msg )
update msg (State state) =
    Editor.Update.update msg state |> Tuple.mapFirst State


view lines (State state) =
    Editor.View.view lines state
