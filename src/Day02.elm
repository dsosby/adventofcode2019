module Day02 exposing (..)

import Array exposing (Array, fromList, get, set, slice, toList)
import Browser
import Element exposing (Element, column, el, row, text, height, width, fill, px, centerX, scrollbarX)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import List exposing (filterMap, isEmpty, map, sum)
import String exposing (fromInt, join, split, toInt, trim)

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

type alias RunningGravityComputer = { ptr : Int, state : Array Int }
type GravityComputer
  = Faulted
  | Running RunningGravityComputer

initialize : String -> RunningGravityComputer
initialize input =
  let
    parsed =
      input
      |> trim
      |> split ","
      |> filterMap toInt
      |> fromList
  in
    { ptr = 0
    , state = parsed }

restore : RunningGravityComputer -> GravityComputer
restore computer =
  Running
    { computer | state =
      computer.state
      |> set 1 12
      |> set 2 2
    }

{- TODO See if I can refactor the input extraction and/or op implementations to be generic and re-usable -}

binaryOperation : (Int -> Int -> Int) -> (Int -> RunningGravityComputer -> GravityComputer)
binaryOperation op =
  \ptr computer ->
    let
      inputs = 
        ( get (ptr + 1) computer.state
        , get (ptr + 2) computer.state
        , get (ptr + 3) computer.state
        )
    in
      case inputs of
        (Just idxLhv, Just idxRhv, Just idxTarget) ->
          let
            maybeLhv = get idxLhv computer.state
            maybeRhv = get idxRhv computer.state
          in
            case (maybeLhv, maybeRhv) of
              (Just lhv, Just rhv) ->
                  Running { computer | state = set idxTarget (op lhv rhv) computer.state, ptr = (ptr + 4) }
              _ -> Faulted {- The indexes were invalid -}
        _ -> Faulted {- The state did not provide enough inputs for this opcode -}

addOperation = binaryOperation (+)
multiplyOperation = binaryOperation (*)

{- TODO Investigate railway oriented error handling like F#
   Faulted state is verbose -}

run : GravityComputer -> GravityComputer
run c =
  case c of
    Faulted -> Faulted
    Running computer ->
      let
        curOp =
          get computer.ptr computer.state
      in
        case curOp of
          Nothing -> Faulted {- Corrupt program detected. I love that Elm makes you deal with these -}
          Just opcode ->
            case opcode of
              1 -> run (addOperation computer.ptr computer)
              2 -> run (multiplyOperation computer.ptr computer)
              99 -> Running computer -- Quit
              _ -> Faulted -- Unknown opcode

print : GravityComputer -> String
print computer =
  case computer of
    Faulted -> "Faulted"
    Running c -> join ", " (toList c.state |> map fromInt)

solve : String -> String
solve puzzleInput =
  initialize puzzleInput
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