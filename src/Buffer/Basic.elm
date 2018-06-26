module Buffer.Basic exposing (Buffer, init, lines, insert, toString)

import List.Extra
import String.Extra
import Editor.Model exposing (Position)


type Buffer
    = Buffer String


init : String -> Buffer
init content =
    Buffer content


listMapAt : (a -> a) -> Int -> List a -> List a
listMapAt fn index list =
    List.Extra.getAt index list
        |> Maybe.map (\a -> List.Extra.setAt index (fn a) list)
        |> Maybe.withDefault list


insert : Position -> String -> Buffer -> Buffer
insert { line, column } string (Buffer buffer) =
    -- I think this could be done more efficiently with a fold; count newlines until
    -- line, then count characters until column. Insert the char, then do nothing to
    -- the rest of the buffer
    String.lines buffer
        |> listMapAt (String.Extra.insertAt string column) line
        |> String.join "\n"
        |> Buffer


lines : Buffer -> List String
lines (Buffer content) =
    String.split "\n" content


toString : Buffer -> String
toString (Buffer buffer) =
    buffer
