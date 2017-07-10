module Main exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (style)


type Move
    = X
    | O


type TicTacToe
    = TicTacToe (Maybe Move) (Maybe Move) (Maybe Move) (Maybe Move) (Maybe Move) (Maybe Move) (Maybe Move) (Maybe Move) (Maybe Move)


emptyBoard : TicTacToe
emptyBoard =
    TicTacToe
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing


draw : Maybe Move -> Html msg
draw move =
    move |> toString |> Html.text


borderStyle =
    style [ ( "border", "1px solid red" ) ]


main : Html Never
main =
    case emptyBoard of
        TicTacToe a1 a2 a3 b1 b2 b3 c1 c2 c3 ->
            Html.table []
                [ Html.tr []
                    [ Html.td [ borderStyle ] [ draw a1 ]
                    , Html.td [ borderStyle ] [ draw a2 ]
                    , Html.td [ borderStyle ] [ draw a3 ]
                    ]
                , Html.tr []
                    [ Html.td [ borderStyle ] [ draw b1 ]
                    , Html.td [ borderStyle ] [ draw b2 ]
                    , Html.td [ borderStyle ] [ draw b3 ]
                    ]
                , Html.tr []
                    [ Html.td [ borderStyle ] [ draw c1 ]
                    , Html.td [ borderStyle ] [ draw c2 ]
                    , Html.td [ borderStyle ] [ draw c3 ]
                    ]
                ]
