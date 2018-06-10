module Buffer.Basic exposing (Buffer, init, lines)


type Buffer
    = Buffer String


init : String -> Buffer
init content =
    Buffer content


lines : Buffer -> List String
lines (Buffer content) =
    String.split "\n" content
