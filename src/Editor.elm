module Editor exposing (State, init, view)

import Html exposing (Html)
import Editor.Model exposing (InternalState, Position)
import Editor.Update exposing (Msg)
import Editor.View


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
view lines (State state) =
    Editor.View.view lines state
