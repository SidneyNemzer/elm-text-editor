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



-- MODEL


type alias Model =
    { editor : Editor.State }


init : ( Model, Cmd Msg )
init =
    { editor = Editor.init "Testing\nTest" } ! []



-- UPDATE


type Msg
    = EditorMsg Editor.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg msg_ ->
            let
                ( updatedEditor, editorCmd ) =
                    Editor.update msg_ model.editor
            in
                { model | editor = updatedEditor } ! [ Cmd.map EditorMsg editorCmd ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
    div []
        [ Editor.Styles.styles
        , Html.map EditorMsg <| Editor.view model.editor
        , div [] [ Html.text <| toString model.editor ]
        ]
