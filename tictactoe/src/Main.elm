module Main exposing (Model, Msg(..), init, main)

import Array
import Browser
import Browser.Navigation as Nav
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    Array.Array Int


init : flag -> ( Model, Cmd Msg )
init _ =
    ( Array.fromList [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ], Cmd.none )



-- UPDATE


type Msg
    = Tap Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tap index ->
            ( Array.set index (getNextValue model) model, Cmd.none )


getNextValue : Array.Array Int -> Int
getNextValue model =
    let
        len =
            Array.filter (\v -> v == 0) model |> Array.length
    in
    if remainderBy 2 len == 0 then
        -1

    else
        1



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewCell 0 model
        , viewCell 1 model
        , viewCell 2 model
        , br [] []
        , viewCell 3 model
        , viewCell 4 model
        , viewCell 5 model
        , br [] []
        , viewCell 6 model
        , viewCell 7 model
        , viewCell 8 model
        ]


viewCell : Int -> Model -> Html Msg
viewCell index model =
    let
        maybe =
            Array.get index model

        value =
            case maybe of
                Just val ->
                    val

                Nothing ->
                    0
    in
    if value == 0 && judge model == None then
        div [ class "s", onClick (Tap index) ] [ getMark value |> text ]

    else
        div [ class "s" ] [ getMark value |> text ]


getMark : Int -> String
getMark val =
    if val == -1 then
        "×"

    else if val == 1 then
        "○"

    else
        "-"


type JudgeResult
    = Maru
    | Batsu
    | None


judge :
    Array.Array Int
    -> JudgeResult
judge model =
    let
        patterns =
            [ ( 0, 1, 2 )
            , ( 3, 4, 5 )
            , ( 6, 7, 8 )
            , ( 0, 3, 6 )
            , ( 1, 4, 7 )
            , ( 2, 5, 8 )
            , ( 0, 4, 8 )
            , ( 2, 4, 6 )
            ]

        judgeList =
            List.map judgeLine (List.map (\v -> getLine v model) patterns)
    in
    if List.length (List.filter (\v -> v == Maru) judgeList) > 0 then
        Maru

    else if List.length (List.filter (\v -> v == Batsu) judgeList) > 0 then
        Batsu

    else
        None


getLine : ( Int, Int, Int ) -> Array.Array Int -> ( Maybe Int, Maybe Int, Maybe Int )
getLine ( index1, index2, index3 ) model =
    ( Array.get index1 model, Array.get index2 model, Array.get index3 model )


judgeLine : ( Maybe Int, Maybe Int, Maybe Int ) -> JudgeResult
judgeLine ( val1, val2, val3 ) =
    let
        sum =
            Maybe.withDefault 0 val1 + Maybe.withDefault 0 val2 + Maybe.withDefault 0 val3
    in
    if sum == -3 then
        Batsu

    else if sum == 3 then
        Maru

    else
        None
