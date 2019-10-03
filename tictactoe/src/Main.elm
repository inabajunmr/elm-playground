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
        [ viewCell (Array.get 0 model)
        , viewCell (Array.get 1 model)
        , viewCell (Array.get 2 model)
        , br [] []
        , viewCell (Array.get 3 model)
        , viewCell (Array.get 4 model)
        , viewCell (Array.get 5 model)
        , br [] []
        , viewCell (Array.get 6 model)
        , viewCell (Array.get 7 model)
        , viewCell (Array.get 8 model)
        ]


viewCell : Maybe Int -> Html Msg
viewCell value =
    case value of
        Nothing ->
            div [] []

        Just v ->
            div [ class "s" ] [ getMark v |> text ]


getMark : Int -> String
getMark val =
    if val == -1 then
        "×"

    else if val == 1 then
        "○"

    else
        "-"
