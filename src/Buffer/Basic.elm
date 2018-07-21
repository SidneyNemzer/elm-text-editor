module Buffer.Basic
    exposing
        ( Buffer
        , init
        , lines
        , insert
        , replace
        , removeBefore
        , toString
        , indentFrom
        , indentSize
        , deindentFrom
        )

import List.Extra
import String.Extra
import Position exposing (Position)


type Buffer
    = Buffer String


indentSize : Int
indentSize =
    2


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


{-| Remove the character before the given position. This is useful because
determining the *previous* valid position is relativly expensive, but it's easy
for the buffer to just use the previous index.
-}
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


{-| Indent the given line from the given column. Returns the modified buffer and
the column + indent size
-}
indentFrom : Position -> Buffer -> ( Buffer, Int )
indentFrom { line, column } (Buffer buffer) =
    indexFromPosition buffer (Position line 0)
        |> Maybe.map
            (\lineStart ->
                let
                    addIndentSize =
                        indentSize
                            - (String.slice lineStart (lineStart + column) buffer
                                |> String.length
                              )
                            % indentSize
                in
                    ( Buffer <|
                        String.slice 0 (lineStart + column) buffer
                            ++ String.repeat addIndentSize " "
                            ++ String.dropLeft (lineStart + column) buffer
                    , column + addIndentSize
                    )
            )
        |> Maybe.withDefault ( Buffer buffer, column )


{-| Deindent the given line from the given column. Returns the modified buffer and
the column - indent size
-}
deindentFrom : Position -> Buffer -> ( Buffer, Int )
deindentFrom { line, column } (Buffer buffer) =
    -- count from lineStart up to indent char while char == indentChar
    -- cut from lineStart to count
    indexFromPosition buffer (Position line 0)
        |> Maybe.map
            (\lineStart ->
                let
                    startChars =
                        String.slice lineStart (lineStart + indentSize) buffer

                    startIndentChars =
                        String.foldl
                            (\char count ->
                                if char == ' ' then
                                    count + 1
                                else
                                    count
                            )
                            0
                            startChars
                in
                    ( Buffer <|
                        String.slice 0 lineStart buffer
                            ++ String.dropLeft (lineStart + startIndentChars) buffer
                    , column - startIndentChars
                    )
            )
        |> Maybe.withDefault ( Buffer buffer, column )
