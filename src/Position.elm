module Position exposing (Position, order, between)


type alias Position =
    { line : Int, column : Int }


order : Position -> Position -> ( Position, Position )
order pos1 pos2 =
    if pos2.line > pos1.line then
        ( pos1, pos2 )
    else
        ( pos2, pos1 )


betweenHelp : Int -> Int -> Int -> Bool
betweenHelp start end point =
    if start > end then
        betweenHelp end start point
    else
        start /= end && point >= start && point < end


between : Position -> Position -> Position -> Bool
between pos1 pos2 { line, column } =
    let
        ( start, end ) =
            order pos1 pos2
    in
        if start.line == end.line then
            line == start.line && betweenHelp start.column end.column column
        else if start.line == line then
            column >= start.column
        else if end.line == line then
            column < end.column
        else
            betweenHelp start.line end.line line
