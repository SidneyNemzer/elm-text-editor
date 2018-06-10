module Editor exposing (State, init, view)

import Html exposing (Html)
import Editor.View as View


type alias InternalState =
    { scroll : Int
    , cursor : ( Int, Int )
    }


type State
    = State InternalState


init : State
init =
    State { scroll = 0, cursor = ( 3, 0 ) }


view : List String -> State -> Html msg
view lines (State state) =
    View.container [] <|
        List.indexedMap (View.line state.cursor) lines
