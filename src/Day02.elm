module Day02 exposing (..)

import Array exposing (Array, fromList, get, set, slice, toList)
import Browser
import Debug exposing (log)
import Element exposing (Element, column, el, row, text, height, width, fill, px, centerX, scrollbarX)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import List exposing (filter, filterMap, head, isEmpty, map, range, sum)
import String exposing (fromInt, join, split, toInt, trim)
import Tuple exposing (first, pair, second)

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

{- Stolen from MartinKavik/elm-combinatorics -}
getVariationsWithReps : Int -> List a -> List (List a)
getVariationsWithReps k set =
    let
        doGetVariationsWithReps mk mset depth resultItem =
            if depth < mk then
                mset
                    |> List.concatMap
                        (\setItem -> doGetVariationsWithReps mk mset (depth + 1) (setItem :: resultItem))
            else
                [ resultItem |> List.reverse ]
    in
    doGetVariationsWithReps k set 0 []

type alias RunningGravityComputer = { ptr : Int, state : Array Int }
type GravityComputer
  = Faulted
  | Running RunningGravityComputer

initialize : String -> Int -> Int -> GravityComputer
initialize input noun verb =
  let
    parsed =
      input
      |> trim
      |> split ","
      |> filterMap toInt
      |> fromList
  in
    Running (restore { ptr = 0, state = parsed } noun verb)

restore : RunningGravityComputer -> Int -> Int -> RunningGravityComputer
restore computer noun verb =
  { computer | state =
    computer.state
    |> set 1 noun
    |> set 2 verb
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

valueAt : Int -> GravityComputer -> Maybe Int
valueAt address computer =
  case computer of
    Faulted -> Nothing
    Running c -> get address c.state

print : GravityComputer -> String
print computer =
  let
    addressZero = valueAt 0 computer
  in
  case addressZero of
    Nothing -> "Faulted"
    Just value -> fromInt value

toTuple : List a -> Maybe (a, a)
toTuple vals =
  case vals of
    [lhv, rhv] -> Just (lhv, rhv)
    _ -> Nothing

{- TODO [Perf] Find/First with Generator would be better, and re-parsing each time could be cached -}
solve : String -> String
solve puzzleInput =
  let
    validInputs = getVariationsWithReps 2 (range 0 99) |> filterMap toTuple
    solution =
      validInputs
      |> map (\nv -> { computer = initialize puzzleInput (first nv) (second nv), noun = first nv, verb = second nv })
      |> map (\r -> { r | computer = run r.computer })
      |> filter (\r -> (log "S" (valueAt 0 r.computer)) == Just 19690720)
      |> head
  in
  case solution of
    Nothing -> "No solution found"
    Just r -> "Noun=" ++ (fromInt r.noun) ++ " Verb=" ++ (fromInt r.verb)

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
      [ Input.multiline [Element.width (Element.px 600), height (px 300), scrollbarX]
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