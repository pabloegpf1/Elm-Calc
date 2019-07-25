module Main exposing (Model, Msg(..), initialModel, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, value)
import Html.Events exposing (..)


type alias Model =
    { value1 : String
    , value2 : String
    , hasComma : Bool
    , lastOp : String
    }


initialModel : Model
initialModel =
    { value1 = "0.0"
    , value2 = ""
    , hasComma = False
    , lastOp = " "
    }


type Msg
    = AddValue1 String
    | AddValue2 String
    | SetTo0
    | AddComma
    | Sum
    | Sub
    | Div
    | Mul
    | Result


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddValue1 value ->
            { model | value1 = model.value1 ++ value }

        AddValue2 value ->
            { model | value2 = model.value2 ++ value }

        SetTo0 ->
            { model
                | value1 = "0.0"
                , value2 = "0.0"
            }

        AddComma ->
            { model
                | value1 =
                    if model.hasComma == True then
                        model.value1 ++ ","

                    else
                        model.value1
                , hasComma = True
            }

        Sum ->
            { model
                | value2 = model.value1
                , value1 = "0.0"
                , lastOp = "+"
            }

        Sub ->
            { model
                | value2 = model.value1
                , value1 = "0.0"
                , lastOp = "-"
            }

        Mul ->
            { model
                | value2 = model.value1
                , value1 = "0.0"
                , lastOp = "x"
            }

        Div ->
            { model
                | value2 = model.value1
                , value1 = "0.0"
                , lastOp = "/"
            }

        Result ->
            { model
                | value1 =
                    if model.lastOp == "+" then
                        String.fromFloat (Maybe.withDefault 0 (String.toFloat model.value2) + Maybe.withDefault 0 (String.toFloat model.value1))

                    else if model.lastOp == "-" then
                        String.fromFloat (Maybe.withDefault 0 (String.toFloat model.value2) - Maybe.withDefault 0 (String.toFloat model.value1))

                    else if model.lastOp == "x" then
                        String.fromFloat (Maybe.withDefault 0 (String.toFloat model.value2) / Maybe.withDefault 0 (String.toFloat model.value1))

                    else if model.lastOp == "/" then
                        String.fromFloat (Maybe.withDefault 0 (String.toFloat model.value2) * Maybe.withDefault 0 (String.toFloat model.value1))

                    else
                        "0.0"
            }


view : Model -> Html.Html Msg
view model =
    div
        [ class "calculator" ]
        [ div
            []
            [ input
                [ onInput AddValue1
                , value model.value1
                ]
                []
            ]
        , div []
            [ button [ class "double", onClick Sum ] [ text "A/C" ]
            , button [ onClick Sub ] [ text "+/-" ]
            , button [ onClick Div ] [ text "÷" ]
            ]
        , div []
            [ button [ onClick (AddValue1 "7") ] [ text "7" ]
            , button [ onClick (AddValue1 "8") ] [ text "8" ]
            , button [ onClick (AddValue1 "9") ] [ text "9" ]
            , button [ onClick Mul ] [ text "x" ]
            ]
        , div []
            [ button [ onClick (AddValue1 "4") ] [ text "4" ]
            , button [ onClick (AddValue1 "5") ] [ text "5" ]
            , button [ onClick (AddValue1 "6") ] [ text "6" ]
            , button [ onClick Sub ] [ text "-" ]
            ]
        , div []
            [ button [ onClick (AddValue1 "1") ] [ text "1" ]
            , button [ onClick (AddValue1 "2") ] [ text "2" ]
            , button [ onClick (AddValue1 "3") ] [ text "3" ]
            , button [ onClick Sum ] [ text "+" ]
            ]
        , div []
            [ button [ onClick (AddValue1 "0") ] [ text "0" ]
            , button [ onClick AddComma ] [ text "," ]
            , button [ class "double", onClick Result ] [ text "=" ]
            ]
        ]


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
