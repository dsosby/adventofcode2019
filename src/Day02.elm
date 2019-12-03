module Day02 exposing (..)

import Array exposing (Array, fromList, get, set)
import Browser
import Element exposing (Element, column, el, row, text, height, width, fill, px, centerX, scrollbarX)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import List exposing (filterMap, isEmpty, map, sum)
import String exposing (join, split, toInt)

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

type GravityComputer
  = Faulted
  | Running { ptr : Int, state : Array Int }

initialize : String -> GravityComputer
initialize input =
  let
    parsed =
      split "," input
      |> filterMap toInt
      |> fromList
  in
    Running
      { ptr = 0
      , state = parsed }

restore : GravityComputer -> GravityComputer
restore computer =
  set 1 12 computer.state
  |> set 2 2 computer.state

addOperation : Int -> GravityComputer -> GravityComputer
addOperation ptr computer =
  let
    a = get (ptr + 1) computer.state
    b = get (ptr + 2) computer.state
    target = get (ptr + 3) computer.state
  in
    {- TODO Can I do tuple matching? -}
    case a of
      Nothing -> Faulted
      Just a ->
        case b of
          Nothing -> Faulted
          Just b -> 
            case target of
              Nothing -> Faulted
              Just target ->
                {- Valid inputs -}
                Running { state | state = set target (a + b) computer.state, ptr = (ptr + 4) }


run : GravityComputer -> GravityComputer
run computer =
  let
    curOp =
      get computer.ptr computer.state
  in
    case curOp of
      Nothing -> Faulted {- Corrupt program detected. Love the Elm makes you deal with these -}
      Just opcode ->
        case opcode of
          1 -> run (addOperation computer) -- Add
          2 -> run (addOperation computer) -- Multiply
          99 -> computer -- Quit

print : GravityComputer -> String
print computer =
  join ", " computer.state

solve : String -> String
solve puzzleInput =
  init puzzleInput
  |> restore
  |> run
  |> print

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
    [ puzzleHeader "Day 02 - Gravity assist computer"
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
        , text <| solve model.inputText
        ]
      ]
    ]