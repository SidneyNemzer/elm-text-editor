module Position
    exposing
        ( Position
        , order
        , between
        , addColumn
        , nextColumn
        , previousColumn
        , setColumn
        , addLine
        , nextLine
        , previousLine
        )


type alias Position =
    { line : Int, column : Int }


order : Position -> Position -> ( Position, Position )
order pos1 pos2 =
    if pos2.line > pos1.line then
        ( pos1, pos2 )
    else if pos2.line == pos1.line && pos2.column > pos1.column then
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


addColumn : Int -> Position -> Position
addColumn amount position =
    { position | column = position.column + amount }


nextColumn : Position -> Position
nextColumn =
    addColumn 1


previousColumn : Position -> Position
previousColumn =
    addColumn -1


setColumn : Int -> Position -> Position
setColumn column position =
    { position | column = column }


addLine : Int -> Position -> Position
addLine amount position =
    { position | line = position.line + amount }


nextLine : Position -> Position
nextLine =
    addLine 1


previousLine : Position -> Position
previousLine =
    addLine -1
