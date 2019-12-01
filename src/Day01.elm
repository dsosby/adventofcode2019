module Day01 exposing (..)

import Browser
import Element exposing (Element, column, el, row, text)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
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

printFuelRequired : FuelRequired -> String
printFuelRequired fuelRequired =
  case fuelRequired of
    InvalidInput -> "Input some masses"
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
  Element.layout [Region.mainContent, Element.width (Element.px 920), Element.padding 30]
    (el [Element.centerX] (puzzleLayout model))

puzzleHeader headerText =
  el [Region.heading 1, Font.size 32, Font.semiBold] (text headerText)
puzzleLabel labelText =
  el [Font.size 16, Font.light] (text labelText)

puzzleLayout model =
  column [Element.width Element.fill, Element.padding 30, Element.spacing 20]
    [ puzzleHeader "Day 01 - Calculate fuel for launch"
    , row [Element.width Element.fill, Element.spacing 20]
      [ Input.multiline [Element.width (Element.fillPortion 2)]
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