module Editor.Keymap exposing (decoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Editor.Update exposing (Msg(..))


type Modifier
    = None
    | Control
    | Shift
    | ControlAndShift


modifier : Bool -> Bool -> Modifier
modifier ctrl shift =
    case ( ctrl, shift ) of
        ( True, True ) ->
            ControlAndShift

        ( False, True ) ->
            Shift

        ( True, False ) ->
            Control

        ( False, False ) ->
            None


modifierDecoder : Decoder Modifier
modifierDecoder =
    Decode.map2 modifier
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "shiftKey" Decode.bool)


decoder : Decoder Msg
decoder =
    modifierDecoder
        |> Decode.andThen
            (\modifier ->
                (Decode.field "key" Decode.string)
                    |> Decode.andThen (keyToMsg modifier)
            )


type alias Keymap =
    Dict String Msg


keymaps :
    { noModifier : Keymap
    , shift : Keymap
    , control : Keymap
    , controlAndShift : Keymap
    }
keymaps =
    { noModifier =
        Dict.fromList
            [ ( "ArrowUp", CursorUp )
            , ( "ArrowDown", CursorDown )
            , ( "ArrowLeft", CursorLeft )
            , ( "ArrowRight", CursorRight )
            , ( "Backspace", RemoveCharBefore )
            , ( "Delete", RemoveCharAfter )
            , ( "Enter", InsertChar '\n' )
            , ( "Home", CursorToStartOfLine )
            , ( "End", CursorToEndOfLine )
            , ( "Tab", IncreaseIndent )
            ]
    , shift =
        Dict.fromList
            [ ( "ArrowUp", SelectUp )
            , ( "ArrowDown", SelectDown )
            , ( "ArrowLeft", SelectLeft )
            , ( "ArrowRight", SelectRight )
            ]
    , control = Dict.empty
    , controlAndShift = Dict.empty
    }


keyToMsg : Modifier -> String -> Decoder Msg
keyToMsg modifier key =
    case String.uncons key of
        Just ( char, "" ) ->
            Decode.succeed (InsertChar char)

        _ ->
            let
                keymap =
                    case modifier of
                        None ->
                            keymaps.noModifier

                        Control ->
                            keymaps.control

                        Shift ->
                            keymaps.shift

                        ControlAndShift ->
                            keymaps.controlAndShift
            in
                Dict.get key keymap
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "This key does nothing")
