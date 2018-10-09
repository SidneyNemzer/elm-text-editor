module Editor.Model exposing (InternalState)

import Editor.History exposing (History)
import Position exposing (Position)


type alias InternalState =
    { scrolledLine : Int
    , cursor : Position
    , selection : Maybe Position
    , dragging : Bool
    , history : History
    }
