module Editor.History exposing (History, Event(..), empty, push, undo)

import Position exposing (Position)


{-| Describes how the text in the buffer changed
-}
type Event
    = Insert { cursor : Position, string : String }
    | Replace
        { start : Position
        , end : Position
        , cursor : Position
        , selection : Maybe Position
        , removed : String
        , inserted : String
        }
    | RemoveBefore { cursor : Position, removed : String }
    | Indent { cursor : Position, moved : Int }
    | IndentLines { cursor : Position, selection : Position }



-- | Deindent { cursor : Position }
-- | DeindentLines { cursor : Position, selection : Position }


{-| Making changes adds entries to the past. Undoing a few times adds those
entries to the future. Making another change adds a new entry to the past and
deletes the future. Redoing moves entries from the future to the past.

Past entries are older at higher indexes, future entries are newer at higher
indexes

-}
type alias InternalHistory =
    { past : List Event, future : List Event }


type History
    = History InternalHistory


empty : History
empty =
    History { past = [], future = [] }


push : Event -> History -> History
push entry (History history) =
    History { past = entry :: history.past, future = [] }


undo : History -> ( History, Maybe Event )
undo (History history) =
    case history.past of
        entry :: past ->
            ( History { past = past, future = entry :: history.future }
            , Just entry
            )

        [] ->
            ( History history, Nothing )
