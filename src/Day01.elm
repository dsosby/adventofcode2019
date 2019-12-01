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

calculateFuelForMass: Int -> Int
calculateFuelForMass mass =
  -- Recur until fuel required is negative
  let fuelRequired = (mass // 3) - 2
  in
    if fuelRequired <= 0
    then 0
    else fuelRequired + (calculateFuelForMass fuelRequired)

calculateFuel : String -> FuelRequired
calculateFuel inputMasses =
  Calculated (
    String.split "\n" (String.trim inputMasses)
    |> map safelyInt -- I should filter out unsafes instead default 0 which yields -2
    |> map calculateFuelForMass
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
