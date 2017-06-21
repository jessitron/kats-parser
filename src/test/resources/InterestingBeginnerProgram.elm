module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode


-- MODEL


type alias Model =
    { labels : List Label
    , newLabel : String
    }


init : Model
init =
    { labels = [ { text = "Russ", x = 200, y = 300 } ]
    , newLabel = ""
    }



-- MESSAGES


type Msg
    = NoOp
    | NewLabel String
    | SaveLabel



-- VIEW


view : Model -> Html Msg
view model =
    Html.main_ []
        [ Html.canvas
            [ Html.Attributes.style
                [ ( "backgroundImage"
                  , "url(" ++ diagram ++ ")"
                  )
                ]
            ]
            []
        , drawLabel model.labels
        , newLabelInput model
        ]


diagram =
    "elmprogram.png"


type alias Label =
    { text : String
    , x : Int
    , y : Int
    }


drawLabel : List Label -> Html Msg
drawLabel labels =
    let
        formatLabel { text, x, y } =
            Html.label
                [ Html.Attributes.style
                    [ ( "position", "absolute" )
                    , ( "top", (toString y) ++ "px" )
                    , ( "left", (toString x) ++ "px" )
                    ]
                ]
                [ Html.text text ]
    in
        Html.div [] (List.map formatLabel labels)


newLabelInput : Model -> Html Msg
newLabelInput model =
    Html.input
        [ Html.Attributes.id "newLabel"
        , Html.Events.onInput NewLabel
        , onEnter SaveLabel
        , Html.Attributes.value model.newLabel
        , Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "top", (toString 400) ++ "px" )
            , ( "left", (toString 200) ++ "px" )
            ]
        ]
        []


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg
            else
                Json.Decode.fail "not ENTER"
    in
        Html.Events.on "keydown" (Json.Decode.andThen isEnter Html.Events.keyCode)



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        NewLabel newLabel ->
            { model | newLabel = newLabel }

        SaveLabel ->
            { model
                | newLabel = ""
                , labels =
                    { text = model.newLabel
                    , x = 400
                    , y = 200
                    }
                        :: model.labels
            }



-- MAIN


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }
