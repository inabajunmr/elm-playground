module Main exposing (Model, Msg(..), Todo, init, main, replace, subscriptions, update, view, viewComment, viewTodo)

import Browser
import Browser.Navigation as Nav
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Url



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
    { todos : List Todo, input : String }


type alias Todo =
    { id : Int, title : String, comments : List String, inputComment : String, status : Bool }


init : flag -> ( Model, Cmd Msg )
init _ =
    ( { todos = [], input = "" }, Cmd.none )



-- UPDATE


type Msg
    = Submit
    | Input String
    | InputComment Int String
    | SubmitComment Int String
    | Done Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            if String.isEmpty model.input then
                ( model, Cmd.none )

            else
                ( { model | todos = Todo (List.length model.todos) model.input [] "" False :: model.todos, input = "" }, Cmd.none )

        Input value ->
            ( { model | input = value }, Cmd.none )

        InputComment index value ->
            ( { model | todos = replace index (\v -> { v | inputComment = value }) model.todos }, Cmd.none )

        SubmitComment index value ->
            if String.isEmpty value then
                ( model, Cmd.none )

            else
                ( { model | todos = replace index (\v -> { v | comments = value :: v.comments, inputComment = "" }) model.todos }, Cmd.none )

        Done index ->
            ( { model | todos = replace index (\v -> { v | status = not v.status }) model.todos }, Cmd.none )


replace : Int -> (a -> a) -> List a -> List a
replace index func list =
    List.indexedMap
        (\i val ->
            if i == index then
                func val

            else
                val
        )
        list



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "c" ]
        [ h1 [] [ text "TodoList" ]
        , input [ value model.input, onInput Input, class "w-100" ] []
        , button [ onClick Submit, class "btn primary" ] [ text "Post Task" ]
        , ul [] (List.indexedMap viewTodo model.todos)
        ]


viewTodo : Int -> Todo -> Html Msg
viewTodo index todo =
    div []
        [ div [ class "card" ]
            [ div [ class "row" ]
                [ h4 [ class "12 col" ] [ text todo.title ]
                ]
            , div [ class "row" ]
                [ input [ class "10 col", class "w-100", onInput (InputComment index), value todo.inputComment ] []
                ]
            , div [ class "row" ]
                [ input [ class "1 col", type_ "checkbox", onClick (Done index), checked todo.status ] []
                , button [ class "4 col", onClick (SubmitComment index todo.inputComment), class "btn primary" ] [ text "Post Comment" ]
                ]
            , ul [] (List.map viewComment todo.comments)
            ]
        ]


viewComment : String -> Html Msg
viewComment comment =
    li [] [ text comment ]
