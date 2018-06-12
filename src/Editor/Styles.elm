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
  user-select: none;
}

.elm-editor-line {
  cursor: text;
}

.elm-editor-line__number {
  display: inline-block;
  width: 20px;
  padding-right: 5px;
  text-align: right;
  background-color: lightgray;
}

.elm-editor-line__content {
  padding-left: 5px;
}

.elm-editor-line__character--has-cursor {
  position: relative;
}

.elm-editor-cursor {
  position: absolute;
  border-left: 1px solid black;
  left: 0;
  height: 100%;
  animation: blink 0.5s infinite alternate;
}

@keyframes blink {
  0% {
    opacity: 1;
  }

  100% {
    opacity: 0;
  }
}
"""


styles : Html msg
styles =
    style [] [ text styleText ]
