module Matematik exposing (..)

import Browser
import Html exposing (Html, div, text, h1)
import Html.Attributes exposing (class)
import Random


main =
    Browser.element { init = init,
    view = view, update = update, subscriptions = (\_ -> Sub.none) }


high : Random.Generator Int
high =
  Random.int 12 120

low : Random.Generator Int
low =
  Random.int 5 40


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0, Random.generate GotRandomNumber (Random.int 0 100000))

-- Model


type alias Model =
    {
    seed : Int
    }




-- Update


type Msg
    = GotRandomNumber Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotRandomNumber x ->
            ({model | seed = x}, Cmd.none)



-- View



view : Model -> Html Msg
view model =
    div [ class "top" ]
        [ h1 [class "no-print"] [ text "Matematik"]
        , div [] (List.map (viewProblem model) (List.range 0 188))
        ]


viewProblem : Model -> Int -> Html Msg
viewProblem model i =
    let
        g = Random.initialSeed (model.seed + i)
        (a, g2)  = (Random.step high g)
        (b, _)  = (Random.step low g2)
    in
        div [ class "problem" ]
            [ div [class "a"] [text (String.fromInt a)]
            , div [class "b"] [text (String.padRight 4 '\u{00A0}' "+" ), text (String.fromInt b)]
            ]
