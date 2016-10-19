import Html exposing (Html, Attribute, button, div, input, text)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String

main =
  App.beginnerProgram { model = model, view = view, update = update }

-- Model

type alias Model =
  { content : String
  , counter : Int
  }
model: Model
model =
  { counter = 0, content = "" }

-- Update

type Msg = Increment | Decrement | Reset | Change String

update: Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      { model | counter = model.counter + 1 }

    Decrement ->
      { model | counter = model.counter - 1 }

    Reset ->
      { model | counter = 0 }

    Change newContent ->
      { model | content = newContent }

-- View

view: Model -> Html Msg
view model =
  div []
    [ button [ onClick Reset ] [ text "reset" ]
    , div [] [ text (toString model.counter) ]
    , button [ onClick Decrement ] [ text "-" ]
    , button [ onClick Increment ] [ text "+" ]
    , div []
      [ input [ placeholder "Enter text to reverse", onInput Change ] []
      , div [] [ text (String.reverse model.content) ]
      ]
    ]
