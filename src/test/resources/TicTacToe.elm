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

