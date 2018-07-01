module Editor.Keymap exposing (decoder)

import Json.Decode as Decode exposing (Decoder)
import Editor.Update exposing (Msg(..))


decoder : Decoder Msg
decoder =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToMsg


keyToMsg : String -> Decoder Msg
keyToMsg key =
    case String.uncons key of
        Just ( char, "" ) ->
            Decode.succeed (InsertChar char)

        _ ->
            case key of
                "ArrowUp" ->
                    Decode.succeed CursorUp

                "ArrowDown" ->
                    Decode.succeed CursorDown

                "ArrowLeft" ->
                    Decode.succeed CursorLeft

                "ArrowRight" ->
                    Decode.succeed CursorRight

                "Backspace" ->
                    Decode.succeed RemoveCharBefore

                "Delete" ->
                    Decode.succeed RemoveCharAfter

                "Enter" ->
                    Decode.succeed (InsertChar '\n')

                _ ->
                    Decode.fail "This key does nothing"
