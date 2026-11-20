port module Matematik exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, h1, input, label, span, table, td, text, tr)
import Html.Attributes exposing (checked, class, name, type_, value)
import Html.Events exposing (onClick, onInput)
import Random
import Set exposing (Set)


main =
    Browser.element
        { init = init
        , view = view
        , update = updateAndSyncUrl
        , subscriptions = \_ -> Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( fromQuery flags.query
        { seed = 0
        , mode = Addition
        , a_range_from = 12
        , a_range_to = 120
        , b_range_from = 5
        , b_range_to = 40
        , tables = Set.empty
        , randomize = False
        , longDivisionStyle =
            if String.startsWith "sv" flags.language then
                SwedishStyle

            else
                UsStyle
        , flags = flags
        }
    , Random.generate GotRandomNumber (Random.int 0 100000)
    )



-- URL sync


port setQuery : String -> Cmd msg


updateAndSyncUrl : Msg -> Model -> ( Model, Cmd Msg )
updateAndSyncUrl msg model =
    let
        ( newModel, cmd ) =
            update msg model
    in
    ( newModel, Cmd.batch [ cmd, setQuery (toQuery newModel) ] )


modeSlug : Mode -> String
modeSlug mode =
    case mode of
        Addition ->
            "addition"

        Subtraction ->
            "subtraction"

        Multiplication ->
            "multiplication"

        Division ->
            "division"

        LongDivision ->
            "long-division"

        MultiplicationTables ->
            "tables"


longDivisionStyleSlug : LongDivisionStyle -> String
longDivisionStyleSlug s =
    case s of
        SwedishStyle ->
            "sv"

        UsStyle ->
            "us"


toQuery : Model -> String
toQuery model =
    let
        params =
            if model.mode == MultiplicationTables then
                ( "tables", String.join "," (List.map String.fromInt (Set.toList model.tables)) )
                    :: (if model.randomize then
                            [ ( "random", "1" ) ]

                        else
                            []
                       )

            else
                [ ( "a_from", String.fromInt model.a_range_from )
                , ( "a_to", String.fromInt model.a_range_to )
                , ( "b_from", String.fromInt model.b_range_from )
                , ( "b_to", String.fromInt model.b_range_to )
                ]
                    ++ (if model.mode == LongDivision then
                            [ ( "style", longDivisionStyleSlug model.longDivisionStyle ) ]

                        else
                            []
                       )
    in
    ( "mode", modeSlug model.mode )
        :: params
        |> List.map (\( k, v ) -> k ++ "=" ++ v)
        |> String.join "&"


parseQuery : String -> Dict String String
parseQuery query =
    (if String.startsWith "?" query then
        String.dropLeft 1 query

     else
        query
    )
        |> String.split "&"
        |> List.filterMap
            (\part ->
                case String.split "=" part of
                    [ k, v ] ->
                        Just ( k, v )

                    _ ->
                        Nothing
            )
        |> Dict.fromList


fromQuery : String -> Model -> Model
fromQuery query model =
    let
        params =
            parseQuery query

        int key default =
            Dict.get key params
                |> Maybe.andThen String.toInt
                |> Maybe.map (clamp 0 500)
                |> Maybe.withDefault default
    in
    { model
        | mode =
            Dict.get "mode" params
                |> Maybe.andThen (\slug -> List.head (List.filter (\m -> modeSlug m == slug) modes))
                |> Maybe.withDefault model.mode
        , a_range_from = int "a_from" model.a_range_from
        , a_range_to = int "a_to" model.a_range_to
        , b_range_from = int "b_from" model.b_range_from
        , b_range_to = int "b_to" model.b_range_to
        , tables =
            case Dict.get "tables" params of
                Just v ->
                    String.split "%2C" v
                        |> List.concatMap (String.split ",")
                        |> List.filterMap String.toInt
                        |> List.filter (\n -> n >= 1 && n <= 15)
                        |> Set.fromList

                Nothing ->
                    model.tables
        , randomize =
            case Dict.get "random" params of
                Just v ->
                    v == "1"

                Nothing ->
                    model.randomize
        , longDivisionStyle =
            Dict.get "style" params
                |> Maybe.andThen (\slug -> List.head (List.filter (\s -> longDivisionStyleSlug s == slug) longDivisionStyles))
                |> Maybe.withDefault model.longDivisionStyle
    }



-- Model


type alias Model =
    { seed : Int
    , mode : Mode
    , a_range_from : Int
    , a_range_to : Int
    , b_range_from : Int
    , b_range_to : Int
    , tables : Set Int
    , randomize : Bool
    , longDivisionStyle : LongDivisionStyle
    , flags : Flags
    }


type LongDivisionStyle
    = SwedishStyle
    | UsStyle


longDivisionStyles : List LongDivisionStyle
longDivisionStyles =
    [ SwedishStyle, UsStyle ]


type alias Flags =
    { language : String
    , query : String
    }


type Mode
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | LongDivision
    | MultiplicationTables


type alias ModeInfo =
    { name : String
    , value : Mode
    }


modes =
    [ Addition
    , Subtraction
    , Multiplication
    , Division
    , LongDivision
    , MultiplicationTables
    ]



-- Update


type Msg
    = GotRandomNumber Int
    | SetMode Mode
    | SetAFrom (Maybe Int)
    | SetATo (Maybe Int)
    | SetBFrom (Maybe Int)
    | SetBTo (Maybe Int)
    | ToggleTable Int
    | ToggleRandomize
    | SetLongDivisionStyle LongDivisionStyle


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

        ToggleTable n ->
            if Set.member n model.tables then
                ( { model | tables = Set.remove n model.tables }, Cmd.none )

            else
                ( { model | tables = Set.insert n model.tables }, Cmd.none )

        ToggleRandomize ->
            ( { model | randomize = not model.randomize }, Cmd.none )

        SetLongDivisionStyle s ->
            ( { model | longDivisionStyle = s }, Cmd.none )

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
            (List.map (viewModeSwitcher model) modes)

        -- settings and problems
        , if model.mode == MultiplicationTables then
            viewTablesSettings model

          else
            viewRangeSettings model
        , if model.mode == LongDivision then
            viewLongDivisionStylePicker model

          else
            text ""
        , if model.mode == MultiplicationTables then
            viewTables model

          else
            div [] (List.map (viewProblem model) (List.range 0 188))
        ]


viewTablesSettings : Model -> Html Msg
viewTablesSettings model =
    div [ class "no-print table-picker" ]
        [ div []
            (List.map
                (\n ->
                    label []
                        [ input [ type_ "checkbox", checked (Set.member n model.tables), onClick (ToggleTable n) ] []
                        , text (String.fromInt n)
                        ]
                )
                (List.range 1 15)
            )
        , div []
            [ label []
                [ input [ type_ "checkbox", checked model.randomize, onClick ToggleRandomize ] []
                , text (randomizeLabel model)
                ]
            ]
        ]


{-| One table per selected number, or when randomizing, all problems from all
tables shuffled into one continuous list.
-}
viewTables : Model -> Html Msg
viewTables model =
    let
        problems =
            Set.toList model.tables
                |> List.concatMap (\n -> List.map (\k -> ( n, k )) (List.range 1 10))
    in
    if model.randomize then
        div [ class "times-list" ]
            (List.map viewListRow (shuffle (Random.initialSeed model.seed) problems))

    else
        div [ class "tables" ] (List.map viewTable (chunksOf 10 problems))


viewListRow : ( Int, Int ) -> Html Msg
viewListRow ( n, k ) =
    div [ class "times-row" ]
        [ span [ class "n" ] [ text (String.fromInt n) ]
        , span [] [ text "·" ]
        , span [ class "k" ] [ text (String.fromInt k) ]
        , span [] [ text "=" ]
        , span [ class "answer" ] []
        ]


shuffle : Random.Seed -> List a -> List a
shuffle seed xs =
    let
        ( keys, _ ) =
            Random.step (Random.list (List.length xs) (Random.float 0 1)) seed
    in
    List.map2 Tuple.pair keys xs
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


chunksOf : Int -> List a -> List (List a)
chunksOf n xs =
    if List.isEmpty xs then
        []

    else
        List.take n xs :: chunksOf n (List.drop n xs)


viewTable : List ( Int, Int ) -> Html Msg
viewTable problems =
    table [ class "times-table" ]
        (List.map
            (\( n, k ) ->
                tr []
                    [ td [ class "n" ] [ text (String.fromInt n) ]
                    , td [] [ text "·" ]
                    , td [ class "k" ] [ text (String.fromInt k) ]
                    , td [] [ text "=" ]
                    , td [ class "answer" ] []
                    ]
            )
            problems
        )


viewRangeSettings : Model -> Html Msg
viewRangeSettings model =
    let
        numberCell lbl v msg =
            td []
                [ label []
                    [ text (lbl ++ " ")
                    , input [ type_ "number", class "range-value", min_ "0", value (String.fromInt v), onInput (msg << String.toInt) ] []
                    ]
                ]

        sliderCell v msg =
            td [] [ input [ type_ "range", class "slider", min_ "0", max_ "500", value (String.fromInt v), onInput (msg << String.toInt) ] [] ]
    in
    table [ class "no-print range-settings" ]
        [ tr [] [ numberCell (fromLabel model) model.a_range_from SetAFrom, numberCell (toLabel model) model.a_range_to SetATo ]
        , tr [] [ sliderCell model.a_range_from SetAFrom, sliderCell model.a_range_to SetATo ]
        , tr [] [ numberCell (fromLabel model) model.b_range_from SetBFrom, numberCell (toLabel model) model.b_range_to SetBTo ]
        , tr [] [ sliderCell model.b_range_from SetBFrom, sliderCell model.b_range_to SetBTo ]
        ]


viewLongDivisionStylePicker : Model -> Html Msg
viewLongDivisionStylePicker model =
    div [ class "no-print style-picker" ]
        (List.map
            (\s ->
                label []
                    [ input [ type_ "radio", name "long-division-style", checked (model.longDivisionStyle == s), onClick (SetLongDivisionStyle s) ] []
                    , text (longDivisionStyleLabel s model)
                    ]
            )
            longDivisionStyles
        )


viewModeSwitcher : Model -> Mode -> Html Msg
viewModeSwitcher model mode =
    div [ class "mode" ]
        [ label []
            [ input [ type_ "radio", name "mode", checked (model.mode == mode), onClick (SetMode mode) ] []
            , text (modeLabel mode model)
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

        a_times_b_s =
            String.fromInt (a * b)

        padLen =
            max (String.length a_s) (String.length b_s) + 1
    in
    case model.mode of
        MultiplicationTables ->
            text ""

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
                [ div [ class "a" ] [ text a_times_b_s, text " / ", text b_s, text " =" ]
                ]

        LongDivision ->
            case model.longDivisionStyle of
                SwedishStyle ->
                    div [ class "problem long-division-sv" ]
                        [ div [ class "both" ] [ span [ class "a" ] [ text a_times_b_s ], span [ class "b" ] [ text b_s ] ]
                        ]

                UsStyle ->
                    div [ class "problem long-division-us" ]
                        [ div [ class "both" ] [ span [ class "b" ] [ text (b_s ++ ")") ], span [ class "a" ] [ text a_times_b_s ] ]
                        ]


min_ : String -> Attribute msg
min_ =
    Html.Attributes.min


max_ : String -> Attribute msg
max_ =
    Html.Attributes.max



--- TRANSLATIONS


modeLabel : Mode -> Model -> String
modeLabel mode model =
    if String.startsWith "sv" model.flags.language then
        case mode of
            Addition ->
                "Addition"

            Subtraction ->
                "Subtraktion"

            Multiplication ->
                "Multiplikation"

            Division ->
                "Division"

            LongDivision ->
                "Uppställd division"

            MultiplicationTables ->
                "Multiplikationstabeller"

    else
        case mode of
            Addition ->
                "Addition"

            Subtraction ->
                "Subtraction"

            Multiplication ->
                "Multiplication"

            Division ->
                "Division"

            LongDivision ->
                "Long division"

            MultiplicationTables ->
                "Multiplication tables"


fromLabel : Model -> String
fromLabel model =
    if String.startsWith "sv" model.flags.language then
        "Från"

    else
        "From"


toLabel : Model -> String
toLabel model =
    if String.startsWith "sv" model.flags.language then
        "Till"

    else
        "To"


longDivisionStyleLabel : LongDivisionStyle -> Model -> String
longDivisionStyleLabel s model =
    if String.startsWith "sv" model.flags.language then
        case s of
            SwedishStyle ->
                "Liggande stolen"

            UsStyle ->
                "Amerikansk"

    else
        case s of
            SwedishStyle ->
                "Swedish"

            UsStyle ->
                "US"


randomizeLabel : Model -> String
randomizeLabel model =
    if String.startsWith "sv" model.flags.language then
        "Blanda"

    else
        "Randomize"
