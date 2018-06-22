module Main exposing (..)

import Html exposing (div, textarea)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Editor
import Editor.Styles
import Buffer.Basic as Buffer


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
    { content : String
    , editor : Editor.State
    }


init : ( Model, Cmd Msg )
init =
    { content = "Test"
    , editor = Editor.init
    }
        ! []



-- UPDATE


type Msg
    = SetContent String
    | EditorMsg Editor.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetContent content ->
            { model | content = content } ! []

        EditorMsg msg_ ->
            let
                ( editor, cmd ) =
                    Editor.update msg_ model.editor
            in
                { model | editor = editor } ! [ Cmd.map EditorMsg cmd ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
    div []
        [ Editor.Styles.styles
        , div [] [ textarea [ value model.content, onInput SetContent ] [] ]
        , model.editor
            |> Editor.view (Buffer.init model.content |> Buffer.lines)
            |> Html.map EditorMsg
        ]
