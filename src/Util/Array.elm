module Util.Array exposing (last)

{-| Basically Array.Extra but for stuff that isn't included in Array.Extra
-}

import Array exposing (Array)


last : Array a -> Maybe ( a, Int )
last array =
    let
        length =
            Array.length array
    in
    Array.slice -1 length array
        |> Array.get 0
        |> Maybe.map (\a -> ( a, length - 1 ))
