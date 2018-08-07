module Main exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Html exposing (div, textarea, text, details, summary)
import Html.Events as Event exposing (onInput)
import Editor
import Editor.Styles
import Buffer exposing (Buffer)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { content : Buffer
    , editor : Editor.State
    , lastKeyPress : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    { content = Buffer.init """listMapAt : (a -> a) -> Int -> List a -> List a
listMapAt fn index list =
  List.Extra.getAt index list
    |> Maybe.map (\\a -> List.Extra.setAt index (fn a) list)
    |> Maybe.withDefault list"""
    , editor = Editor.init
    , lastKeyPress = Nothing
    }
        ! []



-- UPDATE


type Msg
    = EditorMsg Editor.Msg
    | KeyPress String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg msg_ ->
            let
                ( editor, content, cmd ) =
                    Editor.update model.content msg_ model.editor
            in
                { model
                    | editor = editor
                    , content = content
                }
                    ! [ Cmd.map EditorMsg cmd ]

        KeyPress key ->
            { model | lastKeyPress = Just key } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


keyDecoder : (String -> msg) -> Decoder msg
keyDecoder keyToMsg =
    Decode.field "key" Decode.string
        |> Decode.map keyToMsg


view : Model -> Html.Html Msg
view model =
    div
        [ Event.on "keydown" (keyDecoder KeyPress)
        ]
        [ Editor.Styles.styles
        , model.editor
            |> Editor.view model.content
            |> Html.map EditorMsg
        , details []
            [ summary []
                [ text "Debug" ]
            , div [] [ text <| toString model.editor ]
            , case model.lastKeyPress of
                Just key ->
                    div [] [ text <| "Last key press: " ++ key ]

                Nothing ->
                    text ""
            ]
        ]
