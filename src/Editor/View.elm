module Editor.View exposing (container, line)

import Char
import Html exposing (Html, Attribute, text)
import Html.Attributes as Attribute exposing (class)
import Html.Events as Events


type alias Position =
    { line : Int
    , column : Int
    }


name : String
name =
    "elm-editor"


container : List (Attribute msg) -> List (Html msg) -> Html msg
container attrs =
    Html.div ([ class <| name ++ "-container" ] ++ attrs)


nbsp : Char
nbsp =
    Char.fromCode 160


spaceToNbsp : String -> String
spaceToNbsp =
    String.map
        (\char ->
            if char == ' ' then
                nbsp
            else
                char
        )


character : Position -> Bool -> msg -> String -> Html msg
character position hasCursor moveCursorTo content =
    if hasCursor then
        Html.span
            [ class <| name ++ "-line__character--has-cursor"
            , Events.onClick moveCursorTo
            ]
            [ text <| spaceToNbsp content
            , Html.span [ class <| name ++ "-cursor" ] [ text " " ]
            ]
    else
        Html.span
            [ class <| name ++ "-line__character"
            , Events.onClick moveCursorTo
            ]
            [ text <| spaceToNbsp content ]


line : Position -> Int -> (Int -> msg) -> String -> Html msg
line cursor number moveCursorTo content =
    let
        positionedCharacter index =
            character
                { line = number, column = index }
                (cursor.line == number && cursor.column == index)
                (moveCursorTo index)
    in
        Html.div [ class <| name ++ "-line" ] <|
            [ Html.span [ class <| name ++ "-line__number" ]
                [ text <| toString number ]
            , Html.span [ class <| name ++ "-line__content" ]
                (content
                    |> String.split ""
                    |> List.indexedMap positionedCharacter
                )
            ]
                ++ if
                    cursor.line
                        == number
                        && cursor.column
                        >= String.length content
                   then
                    [ Html.span [ class <| name ++ "-line__character--has-cursor" ]
                        [ text " "
                        , Html.span [ class <| name ++ "-cursor" ] [ text " " ]
                        ]
                    ]
                   else
                    []
