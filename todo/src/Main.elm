import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Url
import Debug
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
subscriptions model = Sub.none

-- MODEL
type alias Model = {todos : List Todo, input : String}
type alias Todo = {id : Int, title : String, comments : List String, inputComment : String, status : Bool}

init : flag -> ( Model, Cmd Msg )
init _ =
    ({todos = [], input = ""}, Cmd.none)

-- UPDATE
type Msg = Submit | 
    Input String | 
    InputComment Int String |
    SubmitComment Int String |
    Done Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        Submit
         -> ({ model | todos = Todo (List.length model.todos) model.input [] "" False :: model.todos}, Cmd.none)
        Input value
         -> ({ model | input = value}, Cmd.none)
        InputComment index value
         ->
            ({ model | todos = List.indexedMap (\i todo -> if i == index then { todo | inputComment = value} else todo) model.todos}, Cmd.none)
        SubmitComment index value
         -> let todo = get index model.todos
                in 
                case todo of
                Just a
                    -> let
                        todos = replace index { a | comments = value :: a.comments} model.todos
                    in
                    ({ model | todos = todos}, Cmd.none)    
                Nothing
                     -> (model, Cmd.none)
        Done index
            -> 
            let todo = get index model.todos
                in 
                case todo of
                Just a
                    -> let
                        todos = replace index { a | status = not a.status} model.todos
                    in
                    ({ model | todos = todos}, Cmd.none)    
                Nothing
                     -> (model, Cmd.none)

replace : Int -> a -> List a -> List a
replace index val list = List.take index list ++ [val] ++ List.drop (index + 1) list

put : Int -> a -> List a -> List a
put index val list = List.take index list ++ [val] ++ List.drop index list

get : Int -> List a -> Maybe a
get index list = List.head (List.drop index list)

-- VIEW
view : Model -> Html Msg
view model = 
    div[class "c"][
        h1[][text "TodoList"],
        input[value model.input, onInput Input, class "w-100"][],
        button[onClick Submit, class "btn primary"][text "Post Task"],
        ul[] (List.indexedMap viewTodo model.todos)
        ]

viewTodo : Int -> Todo -> Html Msg
viewTodo index todo =
    Keyed.node "div"[][(String.fromInt todo.id, div[class "card"]
    [
        div[class "row"][
            h4[class "12 col"][text todo.title]
        ],
        div[class "row"][
            input[class "10 col", class "w-100", onInput (InputComment index)][]
        ],
        div[class "row"][
                input [class "1 col", type_ "checkbox", onClick (Done index), checked todo.status ] [],
            button[class "4 col", onClick (SubmitComment index todo.inputComment), class "btn primary"][text "Post Comment"]
    ],
    ul[](List.map viewComment todo.comments)])]

viewComment : String -> Html Msg
viewComment comment = li[][text comment]