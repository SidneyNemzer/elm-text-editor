module Editor.Model exposing (Position, InternalState)


type alias Position =
    { line : Int
    , column : Int
    }


type alias InternalState =
    { scrolledLine : Int
    , cursor : Position
    , selection : Maybe Position
    , dragging : Bool
    }
