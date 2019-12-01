module Day01 exposing (..)

import Browser
import Html exposing (Html, div, label, textarea, text)
import Html.Events exposing (onInput)

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
  div []
    [ label [] [ text "List of module masses" ]
    , textarea [ onInput InputTextUpdated ] [ text model.inputText ]
    , div [] [ text "Calculating... " ]
    ]
