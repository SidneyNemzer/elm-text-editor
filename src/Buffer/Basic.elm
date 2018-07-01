module Buffer.Basic
    exposing
        ( Buffer
        , init
        , lines
        , insert
        , replace
        , removeBefore
        , toString
        )

import List.Extra
import String.Extra
import Position exposing (Position)


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


indexFromPosition : String -> Position -> Maybe Int
indexFromPosition buffer position =
    -- Doesn't validate columns, only lines
    if position.line == 0 then
        Just position.column
    else
        String.indexes "\n" buffer
            |> List.Extra.getAt (position.line - 1)
            |> Maybe.map (\line -> line + position.column + 1)


replace : Position -> Position -> String -> Buffer -> Buffer
replace start end string (Buffer buffer) =
    if
        (start.line > end.line)
            || (start.line == end.line && start.column > end.column)
    then
        replace end start string (Buffer buffer)
    else
        Maybe.map2
            (\startIndex endIndex ->
                String.slice 0 startIndex buffer
                    ++ string
                    ++ String.dropLeft endIndex buffer
            )
            (indexFromPosition buffer start)
            (indexFromPosition buffer end)
            |> Maybe.withDefault buffer
            |> Buffer


removeBefore : Position -> Buffer -> Buffer
removeBefore position (Buffer buffer) =
    indexFromPosition buffer position
        |> Maybe.map
            (\index ->
                String.slice 0 (index - 1) buffer
                    ++ String.dropLeft index buffer
            )
        |> Maybe.withDefault buffer
        |> Buffer


lines : Buffer -> List String
lines (Buffer content) =
    String.split "\n" content


toString : Buffer -> String
toString (Buffer buffer) =
    buffer
