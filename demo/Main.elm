module Main exposing (..)

import Html exposing (div, textarea, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Editor
import Editor.Styles
import Buffer.Basic as Buffer exposing (Buffer)


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
    }


init : ( Model, Cmd Msg )
init =
    { content = Buffer.init """Testsdfasdfa
abcdabasdfaa
acbsdfasdfdf
asdfasdfasdf
asdfasdfasda
asdfasdfasda"""
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
            { model | content = Buffer.init content } ! []

        EditorMsg msg_ ->
            let
                ( editor, cmd ) =
                    Editor.update
                        (Buffer.lines model.content)
                        msg_
                        model.editor
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
        , div []
            [ textarea
                [ value <| Buffer.toString model.content, onInput SetContent ]
                []
            ]
        , model.editor
            |> Editor.view (Buffer.lines model.content)
            |> Html.map EditorMsg
        , div [] [ text <| toString model.editor ]
        ]
