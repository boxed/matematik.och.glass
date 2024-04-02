module Matematik exposing (..)

import Browser
import Html exposing (Attribute, Html, div, h1, input, label, span, text)
import Html.Attributes exposing (checked, class, name, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Random


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seed = 0
      , mode = Addition
      , a_range_from = 12
      , a_range_to = 120
      , b_range_from = 5
      , b_range_to = 40
      }
    , Random.generate GotRandomNumber (Random.int 0 100000)
    )



-- Model


type alias Model =
    { seed : Int
    , mode : Mode
    , a_range_from : Int
    , a_range_to : Int
    , b_range_from : Int
    , b_range_to : Int
    }



-- TODO: division w/ long division (aka "liggande stolen")


type Mode
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | LongDivision


type alias ModeInfo =
    { name : String
    , value : Mode
    }


modes =
    [ { name = "Addition"
      , value = Addition
      }
    , { name = "Subtraktion"
      , value = Subtraction
      }
    , { name = "Multiplikation"
      , value = Multiplication
      }
    , { name = "Division"
      , value = Division
      }
    , { name = "Division, liggande stolen"
      , value = LongDivision
      }
    ]



-- Update


type Msg
    = GotRandomNumber Int
    | SetMode Mode
    | SetAFrom (Maybe Int)
    | SetATo (Maybe Int)
    | SetBFrom (Maybe Int)
    | SetBTo (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandomNumber x ->
            ( { model | seed = x }, Cmd.none )

        SetMode mode ->
            ( { model | mode = mode }, Cmd.none )

        SetAFrom int ->
            case int of
                Just x ->
                    ( { model | a_range_from = x }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetATo int ->
            case int of
                Just x ->
                    ( { model | a_range_to = x }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetBFrom int ->
            case int of
                Just x ->
                    ( { model | b_range_from = x }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetBTo int ->
            case int of
                Just x ->
                    ( { model | b_range_to = x }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ class "top" ]
        [ h1 [ class "no-print" ] [ text "Matematik" ]

        -- mode
        , div [ class "no-print" ]
            (List.map (viewModeSwitcher model.mode) modes)

        -- difficulty settings
        , div [ class "no-print" ]
            [ div [] [ input [ style "width" "400px", type_ "range", min_ "0", max_ "500", value (String.fromInt model.a_range_from), onInput (\x -> SetAFrom (String.toInt x)), class "slider" ] [], text (String.fromInt model.a_range_from) ]
            , div [] [ input [ style "width" "400px", type_ "range", min_ "0", max_ "500", value (String.fromInt model.a_range_to), onInput (\x -> SetATo (String.toInt x)), class "slider" ] [], text (String.fromInt model.a_range_to) ]
            , div [] [ input [ style "width" "400px", type_ "range", min_ "0", max_ "500", value (String.fromInt model.b_range_from), onInput (\x -> SetBFrom (String.toInt x)), class "slider" ] [], text (String.fromInt model.b_range_from) ]
            , div [] [ input [ style "width" "400px", type_ "range", min_ "0", max_ "500", value (String.fromInt model.b_range_to), onInput (\x -> SetBTo (String.toInt x)), class "slider" ] [], text (String.fromInt model.b_range_to) ]
            ]
        , div [] (List.map (viewProblem model) (List.range 0 188))
        ]


viewModeSwitcher : Mode -> ModeInfo -> Html Msg
viewModeSwitcher mode modeInfo =
    div [ class "mode" ]
        [ label []
            [ text modeInfo.name
            , input [ type_ "radio", name "mode", checked (mode == modeInfo.value), onClick (SetMode modeInfo.value) ] []
            ]
        ]


viewProblem : Model -> Int -> Html Msg
viewProblem model i =
    let
        g =
            Random.initialSeed (model.seed + i)

        high : Random.Generator Int
        high =
            Random.int model.a_range_from model.a_range_to

        low : Random.Generator Int
        low =
            Random.int model.b_range_from model.b_range_to

        ( a, g2 ) =
            Random.step high g

        ( b, _ ) =
            Random.step low g2

        a_s =
            String.fromInt a

        b_s =
            String.fromInt b

        padLen =
            max (String.length a_s) (String.length b_s) + 1
    in
    case model.mode of
        Addition ->
            div [ class "problem addition" ]
                [ div [ class "a" ] [ text a_s ]
                , div [ class "b" ] [ text (String.padRight padLen '\u{00A0}' "+"), text b_s ]
                ]

        Subtraction ->
            div [ class "problem subtraction" ]
                [ div [ class "a" ] [ text a_s ]
                , div [ class "b" ] [ text (String.padRight padLen '\u{00A0}' "-"), text b_s ]
                ]

        Multiplication ->
            div [ class "problem multiplication" ]
                [ div [ class "a" ] [ text a_s, text " · ", text b_s, text " =" ]
                ]

        Division ->
            div [ class "problem division" ]
                [ div [ class "a" ] [ text a_s, text " / ", text b_s, text " =" ]
                ]

        LongDivision ->
            div [ class "problem long-division" ]
                [ div [ class "both" ] [ span [ class "a" ] [ text a_s ], span [ class "b" ] [ text b_s ] ]
                ]


min_ : String -> Attribute msg
min_ =
    Html.Attributes.min


max_ : String -> Attribute msg
max_ =
    Html.Attributes.max
