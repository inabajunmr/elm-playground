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
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewCell 1
        , viewCell 0
        , viewCell 0
        , br [] []
        , viewCell 1
        , viewCell -1
        , viewCell 1
        , br [] []
        , viewCell -1
        , viewCell 1
        , viewCell -1
        ]


viewCell : Int -> Html Msg
viewCell value =
    div [ class "s" ] [ getMark value |> text ]


getMark : Int -> String
getMark val =
    if val == -1 then
        "×"

    else if val == 1 then
        "○"

    else
        "-"
