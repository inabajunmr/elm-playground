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
    = Tap Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tap index val ->
            ( Array.set index val model, Cmd.none )



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
        value =
            Array.get index model
    in
    div [ class "s", onClick (Tap index 1) ] [ getMark value |> text ]


getMark : Maybe Int -> String
getMark maybe =
    case maybe of
        Just val ->
            if val == -1 then
                "×"

            else if val == 1 then
                "○"

            else
                "-"

        Nothing ->
            "-"
