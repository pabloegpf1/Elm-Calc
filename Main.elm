module Main exposing (Model, Msg(..), initialModel, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { value1 : Float
    , value2 : Float
    , result : Float
    , lastOp : String
    }


initialModel : Model
initialModel =
    { value1 = 0.0
    , value2 = 0.0
    , result = 0.0
    , lastOp = " "
    }


type Msg
    = SetValue1 String
    | SetValue2 String
    | Sum
    | Sub
    | Div
    | Mul


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetValue1 value ->
            { model | value1 = Maybe.withDefault 0.0 (String.toFloat value) }

        SetValue2 value ->
            { model | value2 = Maybe.withDefault 0.0 (String.toFloat value) }

        Sum ->
            { model
                | result = model.value1 + model.value2
                , lastOp = "+"
            }

        Sub ->
            { model
                | result = model.value1 - model.value2
                , lastOp = "-"
            }

        Mul ->
            { model
                | result = model.value1 * model.value2
                , lastOp = "x"
            }

        Div ->
            { model
                | result = model.value1 / model.value2
                , lastOp = "÷"
            }


view : Model -> Html Msg
view model =
    div
        [ class "calculator" ]
        [ div
            []
            [ input
                [ onInput SetValue1
                , value (String.fromFloat model.value1)
                ]
                []
            , text model.lastOp
            , input [ onInput SetValue2, value (String.fromFloat model.value2) ] []
            , text (" = " ++ String.fromFloat model.result)
            ]
        , div
            []
            [ button [ onClick Sum ] [ text "+" ]
            , button [ onClick Sub ] [ text "-" ]
            , button [ onClick Mul ] [ text "x" ]
            , button [ onClick Div ] [ text "÷" ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
