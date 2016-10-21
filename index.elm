import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)
import String
import Random

main : Program Never
main = App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- Model

type alias Model =
  { content : String
  , counter : Int
  , name : String
  , password : String
  , passwordAgain : String
  , isRevealed : Bool
  , dieFace : Int
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
  | Roll
  | NewFace Int

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      ({ model | counter = model.counter + 1 }, Cmd.none)

    Decrement ->
      ({ model | counter = model.counter - 1 }, Cmd.none)

    Reset ->
      ({ model | counter = 0 }, Cmd.none)

    Change newContent ->
      ({ model | content = newContent }, Cmd.none)

    Name name ->
      ({ model | name = name }, Cmd.none)

    Password password ->
      ({ model | password = password }, Cmd.none)
    
    PasswordAgain password ->
      ({ model | passwordAgain = password }, Cmd.none)

    Reveal doReveal ->
      ({ model | isRevealed = doReveal }, Cmd.none)

    Roll ->
      (model, Random.generate NewFace (Random.int 1 6))

    NewFace newFace ->
      ({model | dieFace = newFace }, Cmd.none)

-- View

view: Model -> Html Msg
view model =
  div []
    [ viewCounter model
    , viewInputWithReverse model
    , viewLoginPassword model
    , viewValidation model
    , viewDice model
    ]


viewCounter: Model -> Html Msg
viewCounter model =
  div []
      [ button [ onClick Reset ] [ text "reset" ]
      , div [] [ text (toString model.counter) ]
      , button [ onClick Decrement ] [ text "-" ]
      , button [ onClick Increment ] [ text "+" ]  
      ]

viewInputWithReverse: Model -> Html Msg
viewInputWithReverse model =
  div []
      [ input [ placeholder "Enter text to reverse", onInput Change ] []
      , div [] [ text (String.reverse model.content) ]
      ]

viewLoginPassword: Model -> Html Msg
viewLoginPassword model =
  div []
      [ input [ type' "text", placeholder "Name", onInput Name ] []
      , input [ type' (if model.isRevealed then "text" else "password"), placeholder "Password", onInput Password ] []
      , input [ type' (if model.isRevealed then "text" else "password"), placeholder "Password Again", onInput PasswordAgain ] []
      , input [ type' "checkbox", onCheck Reveal ] []
      , text "reveal everything!"
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

viewDice : Model -> Html Msg
viewDice model =
  div []
    [ h1 [] [ text (toString model.dieFace) ]
    , button [ onClick Roll ] [ text "Roll!"]
    ]

-- -- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none

-- INIT

init : (Model, Cmd Msg)
init =
  (Model "xxx" 0 "" "" "" False 1, Cmd.none)