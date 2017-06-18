module BeginnerProgramTextInput exposing (main)

import Html exposing (Html)
import Html.Attributes
import Html.Events


-- MODEL


type alias Model =
    { newLabel : String }


init : Model
init =
    { newLabel = "" }



-- MESSAGES


type Msg
    = NoOp
    | NewLabel String



-- VIEW


view : Model -> Html Msg
view model =
    Html.main_ [] [ Html.canvas [ Html.Attributes.style [ ( "backgroundImage", "url(elmbp.png)" ) ] ] [ drawLabel { text = "yes" } ] ]


drawLabel : { text : String } -> Html Msg
drawLabel { text } =
    Html.label [] [ Html.text text ]


newLabelInput : Model -> Html Msg
newLabelInput model =
    Html.input
        [ Html.Attributes.id "newLabel"
        , Html.Events.onInput NewLabel
        , Html.Attributes.value model.newLabel
        ]
        []



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        NewLabel newLabel ->
            { model | newLabel = newLabel }



-- MAIN


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }
