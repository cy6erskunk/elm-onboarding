import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)
import String
import Http
import Json.Decode as Json
import Task
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

main : Program Never
main = App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- Model

type alias Model =
  { counter : Int
  , name : String
  , password : String
  , passwordAgain : String
  , isRevealed : Bool
  , dieFace : Int
  , url : String
  , reqError : String
  , someResponse : String
  , time : Time
  , startTime : Time
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
  | Tick Time

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
          ({ model | reqError = (toString code) ++ ": " ++ str }, Cmd.none)

    FetchSucceed str ->
      ({ model | someResponse = str, reqError = "" }, Cmd.none)

    Tick newTime ->
      ({ model
      | startTime = if model.startTime == 0 then newTime else model.startTime
      , time = if model.startTime == 0 then 1 * Time.second else newTime - model.startTime + 1 * Time.second
      }, Cmd.none)

-- View

view: Model -> Html Msg
view model =
  div []
    [ viewClock model
    , viewLoginPassword model
    , viewValidation model
    , viewHttpReq model
    ]


viewClock : Model -> Html Msg
viewClock model =
  let
    angle =
      turns (Time.inMinutes model.time - 1/4)

    handX =
      toString (50 + 40 * cos angle)

    handY =
      toString (50 + 40 * sin angle)
  in
    Svg.svg [ viewBox "0 0 100 100", Svg.Attributes.width "100px"]
      [ circle [ cx "50", cy "50", r "45", fill "#0b79ce" ] []
      , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
      , Svg.text' [ x "25", y "45", fontSize "8px" ] [ Svg.text (toString <| model.time) ]
      ]

viewLoginPassword: Model -> Html Msg
viewLoginPassword model =
  div []
      [ input [ Html.Attributes.type' "text", placeholder "Name", onInput Name ] []
      , input [ Html.Attributes.type' (if model.isRevealed then "text" else "password"), placeholder "Password", onInput Password ] []
      , input [ Html.Attributes.type' (if model.isRevealed then "text" else "password"), placeholder "Password Again", onInput PasswordAgain ] []
      , input [ Html.Attributes.type' "checkbox", onCheck Reveal ] []
      , Html.text "reveal everything!"
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
    div [ Html.Attributes.style [("color", color)] ] [ Html.text message ]

viewHttpReq : Model -> Html Msg
viewHttpReq model =
  div []
    [ input [ onInput UpdateUrl, value model.url ] []
    , Html.text (toString model.url)
    , button [ onClick SendRequest ] [ Html.text "Go!" ]
    , div [] [ Html.text model.reqError ]
    , div [] [ Html.text model.someResponse ]
    ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

-- INIT

init : (Model, Cmd Msg)
init =
  (Model 0 "" "" "" False 1 "http://localhost/" "" "" 0 0, Cmd.none)

-- HTTP
fetchData : String -> Cmd Msg
fetchData url =
-- Http.send : Settings -> Request -> Task RawError Response
-- fromJson : Decoder a -> Task RawError Response -> Task Error a
  Task.perform FetchFail FetchSucceed <| Http.fromJson decodeResponseJson <| Http.send Http.defaultSettings {
    verb = "GET"
  , headers = [("Accept", "application/json")]
  , url = url
  , body = Http.empty
  }

decodeResponseJson : Json.Decoder String
decodeResponseJson =
  Json.at [ "href"] Json.string
