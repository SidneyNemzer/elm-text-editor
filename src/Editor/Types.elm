module Editor.Types exposing (Hover(..), Selection(..), Position, Msg(..), InternalState)

import Array.Hamt as Array exposing (Array)


type Hover
    = NoHover
    | HoverLine Int
    | HoverChar Position


type Selection
    = NoSelection
    | SelectingFrom Hover
    | Selection Position Position


type alias Position =
    { line : Int
    , column : Int
    }


type Msg
    = NoOp
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | NewLine
    | InsertChar Char
    | RemoveCharBefore
    | RemoveCharAfter
    | Hover Hover
    | GoToPosition Position
    | StartSelecting
    | StopSelecting


type alias InternalState =
    { lines : Array String
    , cursor : Position
    , hover : Hover
    , selection : Selection
    }
