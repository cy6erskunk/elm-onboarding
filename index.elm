import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)
import String
import Http
import Random
import Json.Decode as Json
import Task

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
  , url : String
  , reqError : String
  }

-- Update

type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Reveal Bool
  | UpdateUrl String
  | SendRequest
  | FetchFail Http.Error
  | FetchSucceed String

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Name name ->
      ({ model | name = name }, Cmd.none)

    Password password ->
      ({ model | password = password }, Cmd.none)

    PasswordAgain password ->
      ({ model | passwordAgain = password }, Cmd.none)

    Reveal doReveal ->
      ({ model | isRevealed = doReveal }, Cmd.none)

    UpdateUrl newUrl ->
      ({ model | url = newUrl }, Cmd.none)

    SendRequest ->
      (model, fetchData model.url)

    FetchFail error ->
      case error of
        Http.Timeout ->
          ({ model | reqError = "Timeout happened :(" }, Cmd.none)
        Http.NetworkError ->
          ({ model | reqError = "NetworkError :'(" }, Cmd.none)
        Http.UnexpectedPayload s ->
          ({ model | reqError = "UnexpectedPayload: " ++ s }, Cmd.none)
        Http.BadResponse code str ->
          ({ model | reqError = (toString code) ++ str }, Cmd.none)

    FetchSucceed _ ->
      (model, Cmd.none)

-- View

view: Model -> Html Msg
view model =
  div []
    [ viewLoginPassword model
    , viewValidation model
    , viewHttpReq model
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

viewHttpReq : Model -> Html Msg
viewHttpReq model =
  div []
    [ input [ onInput UpdateUrl, value model.url ] []
    , text (toString model.url)
    , button [ onClick SendRequest ] [ text "Go!" ]
    , div [] [ text model.reqError ]
    ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- INIT

init : (Model, Cmd Msg)
init =
  (Model "xxx" 0 "" "" "" False 1 "http://localhost/" "", Cmd.none)

-- HTTP
fetchData : String -> Cmd Msg
fetchData url =
  Task.perform FetchFail FetchSucceed (Http.get decodeResponseJson url)

decodeResponseJson : Json.Decoder String
decodeResponseJson =
  Json.at [ "id", "name"] Json.string
