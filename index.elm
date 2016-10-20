import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)
import String

main : Program Never
main =
  App.beginnerProgram { model = model, view = view, update = update }

-- Model

type alias Model =
  { content : String
  , counter : Int
  , name : String
  , password : String
  , passwordAgain : String
  , isRevealed: Bool
  }
model: Model
model =
  { counter = 0
  , content = "" 
  , name = ""
  , password = ""
  , passwordAgain = ""
  , isRevealed = False 
  }

-- Update

type Msg 
  = Increment
  | Decrement
  | Reset
  | Change String
  | Name String
  | Password String
  | PasswordAgain String
  | Reveal Bool

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

    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }
    
    PasswordAgain password ->
      { model | passwordAgain = password }

    Reveal doReveal ->
      { model | isRevealed = doReveal }

-- View

view: Model -> Html Msg
view model =
  div []
    [ div []
      [ button [ onClick Reset ] [ text "reset" ]
      , div [] [ text (toString model.counter) ]
      , button [ onClick Decrement ] [ text "-" ]
      , button [ onClick Increment ] [ text "+" ]  
      ]
    , div []
      [ input [ placeholder "Enter text to reverse", onInput Change ] []
      , div [] [ text (String.reverse model.content) ]
      ]
    , div []
      [ input [ type' "text", placeholder "Name", onInput Name ] []
      , input [ type' (if model.isRevealed then "text" else "password"), placeholder "Password", onInput Password ] []
      , input [ type' (if model.isRevealed then "text" else "password"), placeholder "Password Again", onInput PasswordAgain ] []
      , input [ type' "checkbox", onCheck Reveal ] []
      , text "reveal everything!"
      ]
    , viewValidation model
    ]

viewValidation : Model -> Html Msg
viewValidation model =
  let
    (color, message) =
      if model.password == model.passwordAgain then
        if (String.length model.password < 4) then
          ("salmon", "so weak, sol languid...")
        else
          ("green", "OK")
      else
        ("red", "Are you a terrorist or what?")
  in
    div [ style [("color", color)] ] [ text message ]