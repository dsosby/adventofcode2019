module Day01 exposing (..)

import Browser
import Element exposing (Element, column, el, row, text, height, width, fill, px, centerX, scrollbarX)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import List exposing (filterMap, isEmpty, map, sum)

main =
  Browser.sandbox { init = init, update = update, view = view }

-- Structure

type FuelRequired =
 InvalidInput
 | Calculated Int

type alias Model =
  { inputText: String
  , outputValue: FuelRequired
  }

init : Model
init =
  { inputText = ""
  , outputValue = InvalidInput
  }

-- Solver

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
  let
    fuelCalculations =
      String.split "\n" (String.trim inputMasses)
      |> filterMap String.toInt
      |> map calculateFuelForMass
  in
    if (isEmpty fuelCalculations)
    then InvalidInput
    else Calculated <| sum fuelCalculations

printFuelRequired : FuelRequired -> String
printFuelRequired fuelRequired =
  case fuelRequired of
    InvalidInput -> "?"
    Calculated fuel -> String.fromInt fuel

getSolution : String -> String
getSolution puzzleInput =
  printFuelRequired <| calculateFuel puzzleInput

-- Update

type Msg =
  InputTextUpdated String

update : Msg -> Model -> Model
update msg model =
  case msg of
    InputTextUpdated newInputText ->
      { model | inputText = newInputText }

-- View

view : Model -> Html Msg
view model =
  Element.layout [Region.mainContent, Element.padding 30]
    <| centeredColumn
    <| puzzle model

centeredColumn x =
    row [ width fill]
        <| [ el [ centerX, width (px 960) ] x ]

puzzleHeader headerText =
  el [Region.heading 1, Font.size 32, Font.semiBold] (text headerText)
puzzleLabel labelText =
  el [Font.size 16, Font.light] (text labelText)

puzzle model =
  column [Element.width Element.fill, Element.padding 30, Element.spacing 20]
    [ puzzleHeader "Day 01 - Calculate fuel for launch"
    , row [Element.width Element.fill, Element.spacing 20]
      [ Input.multiline [Element.width (Element.fillPortion 2), height (px 300), scrollbarX]
        { onChange = InputTextUpdated
        , spellcheck = False
        , label = Input.labelAbove [] (puzzleLabel "Puzzle input:")
        , placeholder = Just (Input.placeholder [] (text "Paste the puzzle input"))
        , text = model.inputText
        }
      , column [Element.width (Element.fillPortion 1), Element.spacing 20, Element.alignTop] 
        [ (puzzleLabel "Solution:")
        , text <| getSolution model.inputText
        ]
      ]
    ]