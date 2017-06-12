module BeginnerProgram exposing (..)

import Html exposing (Html)


-- MODEL


type alias Model =
    { count: Int , messages: List String }


init : Model
init =
    { count = 0 , messages = [] }



-- MESSAGES


type Msg
    = NoOp
    | Yes String



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [] []



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Yes string ->
            { model | messages = [string] }
        NoOp ->
            model



-- MAIN


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }
