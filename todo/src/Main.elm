import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Url
import Debug

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
type alias Todo = {title : String, comments : List String, inputComment : String, status : Bool}

init : flag -> ( Model, Cmd Msg )
init _ =
    ({todos = [], input = ""}, Cmd.none)

-- UPDATE
type Msg = Submit | 
    Input String | 
    InputComment Int String |
    SubmitComment Int String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        Submit
         -> ({ model | todos = Todo model.input [] "" False :: model.todos}, Cmd.none)
        Input value
         -> ({ model | input = value}, Cmd.none)
        InputComment index value
         -> let todo = get index model.todos
                in 
                case todo of
                Just a
                    -> let
                        todos = replace index { a | inputComment = value} model.todos
                    in
                    ({ model | todos = todos}, Cmd.none)    
                Nothing
                     -> (model, Cmd.none)
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
        input[value model.input, onInput Input][],
        button[onClick Submit][text "submit"],
        ul[] (List.indexedMap viewTodo model.todos)
        ]

viewTodo : Int -> Todo -> Html Msg
viewTodo index todo = div[class "card"]
    [h4[][text todo.title],
    input[onInput (InputComment index)][],
    button[onClick (SubmitComment index todo.inputComment)][text "submit"],
    ul[](List.map viewComment todo.comments)]

viewComment : String -> Html Msg
viewComment comment = li[][text comment]