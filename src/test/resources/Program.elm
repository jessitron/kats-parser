module Program exposing (main)

import Html exposing (Html)
import Mouse


-- MODEL


type alias Model =
    { count: Int, thing: Maybe String }


init : ( Model, Cmd Msg )
init =
    ( { count = 0, thing = Nothing }, Cmd.none )



-- MESSAGES


type Msg
    = NoOp
    | Something String
    | Click ( Maybe Mouse.Position )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [] []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Something string ->
            ( { model | thing = Just string }, Cmd.none )

        Click Nothing ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
