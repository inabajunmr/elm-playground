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
type alias Model = {todos : List String, input : String}
type alias TODO = {title : String}

init : flag -> ( Model, Cmd Msg )
init _ =
    ({todos = [], input = ""}, Cmd.none)

-- UPDATE
type Msg = Submit | Input String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        Submit -> ({ model | todos = model.input :: model.todos}, Cmd.none)
        Input value -> ({ model | input = value}, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model = 
    div[][
        input[value model.input, onInput Input][],
        button[onClick Submit][text "submit"],
        ul[] (List.map viewTodo model.todos)
        ]

viewTodo : String -> Html Msg
viewTodo todo = li[][text todo]