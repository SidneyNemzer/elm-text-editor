module Editor.Styles exposing (styles)

import Html exposing (Html, text)


style : List (Html.Attribute msg) -> List (Html msg) -> Html msg
style =
    Html.node "style"


styleText : String
styleText =
    """
.elm-editor-container {
  font-family: monospace;
  border: 1px solid lightgray;
  width: 700px;
}

.elm-editor-line__number {
  display: inline-block;
  width: 20px;
  padding-right: 5px;
  text-align: right;
  background-color: lightgray;
}

.elm-editor-line__content {
}

.elm-editor-line__content--has-cursor {
  position: relative;
}

.elm-editor-cursor {
  position: absolute;
  border-left: 1px solid black;
  left: 0;
  height: 100%;
  animation: blink 1s infinite;
}

@keyframes blink {
  0% {
    opacity: 0;
  }

  100% {
    opacity: 1;
  }
}
"""


styles : Html msg
styles =
    style [] [ text styleText ]
