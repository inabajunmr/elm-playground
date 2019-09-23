import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
subscriptions model = Sub.none

-- MODEL
type alias Model = {todos : List Todo, input : String}
type alias Todo = {id : Int, title : String, comments : List String, status : Bool}

init : flag -> ( Model, Cmd Msg )
init _ =
    ({todos = [], input = ""}, Cmd.none)

-- UPDATE
type Msg = Submit | Input String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        Submit -> ({ model | todos = Todo (List.length model.todos) model.input [] False :: model.todos}, Cmd.none)
        Input value -> ({ model | input = value}, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model = 
    div[class "c"][
        input[value model.input, onInput Input][],
        button[onClick Submit][text "submit"],
        ul[] (List.map viewTodo model.todos)
        ]

viewTodo : Todo -> Html Msg
viewTodo todo = div[class "card"]
    [h4[][text todo.title],
    ul[](List.map viewComment todo.comments)]

viewComment : String -> Html Msg
viewComment comment = li[][text comment]