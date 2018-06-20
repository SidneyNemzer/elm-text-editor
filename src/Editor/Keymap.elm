module Editor.Keymap exposing (keyDecoder)

import Json.Decode as Decode exposing (Decoder)
import Editor.Types exposing (Msg(..))


keyDecoder : Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToMsg


keyToMsg : String -> Decoder Msg
keyToMsg string =
    case String.uncons string of
        Just ( char, "" ) ->
            Decode.succeed (InsertChar char)

        _ ->
            case string of
                "ArrowUp" ->
                    Decode.succeed MoveUp

                "ArrowDown" ->
                    Decode.succeed MoveDown

                "ArrowLeft" ->
                    Decode.succeed MoveLeft

                "ArrowRight" ->
                    Decode.succeed MoveRight

                "Backspace" ->
                    Decode.succeed RemoveCharBefore

                "Delete" ->
                    Decode.succeed RemoveCharAfter

                "Enter" ->
                    Decode.succeed NewLine

                _ ->
                    Decode.fail "This key does nothing"
