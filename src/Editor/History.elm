module Editor.History exposing (History, empty, push, redo, undo)

{-| Making changes adds entries to the past. Undoing a few times adds those
entries to the future. Making another change adds a new entry to the past and
deletes the future. Redoing moves entries from the future to the past.
Past entries are older at higher indexes, future entries are newer at higher
indexes
-}


type alias InternalHistory a =
    { past : List a, future : List a }


type History a
    = History (InternalHistory a)


empty : History a
empty =
    History { past = [], future = [] }


push : a -> History a -> History a
push entry (History history) =
    History { past = entry :: history.past, future = [] }


undo : a -> History a -> Maybe ( History a, a )
undo current (History history) =
    case history.past of
        previous :: past ->
            Just
                ( History { past = past, future = current :: history.future }
                , previous
                )

        [] ->
            Nothing


redo : a -> History a -> Maybe ( History a, a )
redo current (History history) =
    case history.future of
        next :: future ->
            Just
                ( History { past = current :: history.past, future = future }
                , next
                )

        [] ->
            Nothing
