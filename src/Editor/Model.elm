module Editor.Model exposing (InternalState)

import Position exposing (Position)
import Editor.History exposing (History)


type alias InternalState =
    { scrolledLine : Int
    , cursor : Position
    , selection : Maybe Position
    , dragging : Bool
    , history : History
    }
