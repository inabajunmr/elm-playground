port module Main exposing (Model, Msg(..), Todo, init, main, replace, subscriptions, update, view, viewComment, viewTodo)

import Browser
import Browser.Navigation as Nav
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode exposing (Decoder, bool, field, string)
import Json.Encode
import Url



-- MAIN


main : Program Json.Encode.Value Model Msg
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
    { title : String, comments : List String, inputComment : String, status : Bool }


init : Json.Encode.Value -> ( Model, Cmd Msg )
init data =
    let
        result =
            Json.Decode.decodeValue todosDecoder data
    in
    case result of
        Ok val ->
            ( { todos = val, input = "" }, Cmd.none )

        Err error ->
            ( { todos = [], input = "" }, Cmd.none )


todosDecoder : Json.Decode.Decoder (List Todo)
todosDecoder =
    Json.Decode.list todoDecoder


todoDecoder : Json.Decode.Decoder Todo
todoDecoder =
    Json.Decode.map4 Todo
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "comments" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "inputComment" Json.Decode.string)
        (Json.Decode.field "status" Json.Decode.bool)



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
                let
                    updateModel =
                        { model | todos = Todo model.input [] "" False :: model.todos, input = "" }
                in
                ( updateModel, saveData (encodeTodos updateModel.todos) )

        Input value ->
            ( { model | input = value }, Cmd.none )

        InputComment index value ->
            ( { model | todos = replace index (\v -> { v | inputComment = value }) model.todos }, Cmd.none )

        SubmitComment index value ->
            if String.isEmpty value then
                ( model, Cmd.none )

            else
                let
                    updateModel =
                        { model | todos = replace index (\v -> { v | comments = value :: v.comments, inputComment = "" }) model.todos }
                in
                ( updateModel, saveData (encodeTodos updateModel.todos) )

        Done index ->
            let
                updateModel =
                    { model | todos = replace index (\v -> { v | status = not v.status }) model.todos }
            in
            ( updateModel, saveData (encodeTodos updateModel.todos) )


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


encodeTodos : List Todo -> Json.Encode.Value
encodeTodos list =
    Json.Encode.list encodeTodo list


encodeTodo : Todo -> Json.Encode.Value
encodeTodo todo =
    Json.Encode.object
        [ ( "title", Json.Encode.string todo.title )
        , ( "comments", Json.Encode.list Json.Encode.string todo.comments )
        , ( "status", Json.Encode.bool todo.status )
        , ( "inputComment", Json.Encode.string todo.inputComment )
        ]


port saveData : Json.Encode.Value -> Cmd msg



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "c" ]
        [ h1 [] [ text "TodoList" ]
        , input [ value model.input, onInput Input, class "card w-100", placeholder "PUT YOUR TODO TITLE" ] []
        , button [ onClick Submit, class "btn primary" ] [ text "Post Task" ]
        , div [] (List.indexedMap viewTodo model.todos)
        ]


viewTodo : Int -> Todo -> Html Msg
viewTodo index todo =
    div []
        [ div [ class "card" ]
            [ div [ class "row" ]
                [ h4 [ class "12 col" ] [ text todo.title ]
                ]
            , div [ class "row" ]
                [ input [ class "10 col", class "card w-100", onInput (InputComment index), value todo.inputComment, placeholder "PUT COMMENT" ] []
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
