module Editor.Model exposing (InternalState, Snapshot)

import Buffer exposing (Buffer)
import Editor.History exposing (History)
import Position exposing (Position)


type alias Snapshot =
    { cursor : Position
    , selection : Maybe Position
    , buffer : Buffer
    }


type alias InternalState =
    { scrolledLine : Int
    , cursor : Position
    , selection : Maybe Position
    , dragging : Bool
    , history : History Snapshot
    }
