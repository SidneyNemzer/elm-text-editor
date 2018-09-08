module Editor.History exposing (History, State, empty, push, undo, redo)

import Position exposing (Position)
import Buffer exposing (Buffer)


type alias State =
    { cursor : Position
    , selection : Maybe Position
    , buffer : Buffer
    }


{-| Making changes adds entries to the past. Undoing a few times adds those
entries to the future. Making another change adds a new entry to the past and
deletes the future. Redoing moves entries from the future to the past.
Past entries are older at higher indexes, future entries are newer at higher
indexes
-}
type alias InternalHistory =
    { past : List State, future : List State }


type History
    = History InternalHistory


empty : History
empty =
    History { past = [], future = [] }


push : State -> History -> History
push entry (History history) =
    History { past = entry :: history.past, future = [] }


undo : State -> History -> ( History, Maybe State )
undo current (History history) =
    case history.past of
        previous :: past ->
            ( History { past = past, future = current :: history.future }
            , Just previous
            )

        [] ->
            ( History history, Nothing )


redo : State -> History -> ( History, Maybe State )
redo current (History history) =
    case history.future of
        next :: future ->
            ( History { past = current :: history.past, future = future }
            , Just next
            )

        [] ->
            ( History history, Nothing )
