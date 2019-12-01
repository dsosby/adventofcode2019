module Day01 exposing (..)

import Browser
import Html exposing (Html, div, label, textarea, text)
import Html.Attributes exposing (id, for, placeholder, rows, cols)
import Html.Events exposing (onInput)
import List exposing (foldl, map)

main =
  Browser.sandbox { init = init, update = update, view = view }

-- Structure

type FuelRequired =
 InvalidInput
 | Calculated Int

type alias Model =
  { inputText: String, outputValue: FuelRequired }

init : Model
init =
  { inputText = "", outputValue = InvalidInput }

-- Solver
safelyInt : String -> Int
safelyInt inputStr = String.toInt inputStr |> Maybe.withDefault 0
divideIntBy x a = a // x

calculateFuel : String -> FuelRequired
calculateFuel inputMasses =
  -- Split by line
  -- Map to Int
  -- Map to calculated fuel (divide by 3, round down, subtract 2)
  -- Reduce by summing
  Calculated (
    String.split "\n" inputMasses
    |> map safelyInt -- I should filter out unsafes instead of which yields -2
    |> map (divideIntBy 3)
    |> map ((+) -2)
    |> foldl (+) 0
  )

-- Update

type Msg =
  InputTextUpdated String

update : Msg -> Model -> Model
update msg model =
  case msg of
    InputTextUpdated newInputText ->
      { model | inputText = newInputText }

-- View

printFuelRequired : FuelRequired -> String
printFuelRequired fuelRequired =
  case fuelRequired of
    InvalidInput -> "Input some masses"
    Calculated fuel -> String.fromInt fuel

view : Model -> Html Msg
view model =
  div []
    [ label [ for "inputArea" ] [ text "List of module masses" ]
    , textarea [ id "inputArea", onInput InputTextUpdated, rows 25, cols 80 ] [ text model.inputText ]
    , div [] [ text (printFuelRequired (calculateFuel model.inputText)) ]
    ]
