module Todo (..) where

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into four distinct parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML
  4. Inputs - the signals necessary to manage events

This clean division of concerns is a core part of Elm. You can read more about
this in the Pong tutorial: http://elm-lang.org/blog/making-pong

This program is not particularly large, so definitely see the following
for notes on structuring more complex GUIs with Elm:
https://github.com/evancz/elm-architecture-tutorial/
-}

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Http
import Json.Decode.Extra exposing ((|:))
import Json.Decode exposing ((:=))
import Signal exposing (Signal, Address)
import StartApp
import String
import Task exposing (andThen)
import Window


---- MODEL ----
-- The full application state of our todo app.


type alias Model =
  { todos : List Todo
  , field : String
  , order : Int
  , visibility : String
  }


type alias Todo =
  { description : String
  , completed : Bool
  , editing : Bool
  , id : Int
  }


newTodo : String -> Int -> Todo
newTodo desc id =
  { description = desc
  , completed = False
  , editing = False
  , id = id
  }


emptyModel : Model
emptyModel =
  { todos = []
  , visibility = "All"
  , field = ""
  , order = 0
  }



---- UPDATE ----
-- A description of the kinds of actions that can be performed on the model of
-- our application. See the following for more info on this pattern and
-- some alternatives: https://github.com/evancz/elm-architecture-tutorial/


type Action
  = NoOp
  | UpdateField String
  | EditingTodo Int Bool
  | UpdateTodo Int String
  | Add
  | Delete Int
  | DeleteComplete
  | Check Int Bool
  | CheckAll Bool
  | ChangeVisibility String
  | InitializeState (List Todo)



-- How we update our Model on a given Action?


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    Add ->
      ( { model
          | order = model.order + 1
          , field = ""
          , todos =
              if String.isEmpty model.field then
                model.todos
              else
                model.todos ++ [ newTodo model.field model.order ]
        }
      , Effects.none
      )

    UpdateField str ->
      ( { model | field = str }, Effects.none )

    EditingTodo id isEditing ->
      let
        updateTodo t =
          if t.id == id then
            { t | editing = isEditing }
          else
            t

        effect =
          if isEditing then
            Signal.send focusIdMailbox.address id
              |> Task.map (always NoOp)
              |> Effects.task
          else
            Effects.none
      in
        ( { model | todos = List.map updateTodo model.todos }, effect )

    UpdateTodo id task ->
      let
        updateTodo t =
          if t.id == id then
            { t | description = task }
          else
            t
      in
        ( { model | todos = List.map updateTodo model.todos }, Effects.none )

    Delete id ->
      ( { model | todos = List.filter (\t -> t.id /= id) model.todos }, Effects.none )

    DeleteComplete ->
      ( { model | todos = List.filter (not << .completed) model.todos }, Effects.none )

    Check id isCompleted ->
      let
        updateTodo t =
          if t.id == id then
            { t | completed = isCompleted }
          else
            t
      in
        ( { model | todos = List.map updateTodo model.todos }, Effects.none )

    CheckAll isCompleted ->
      let
        updateTodo t =
          { t | completed = isCompleted }
      in
        ( { model | todos = List.map updateTodo model.todos }, Effects.none )

    ChangeVisibility visibility ->
      ( { model | visibility = visibility }, Effects.none )

    InitializeState todos ->
      ( { model | todos = todos }, Effects.none )



---- VIEW ----


view : Address Action -> Model -> Html
view address model =
  div
    [ class "todomvc-wrapper"
    , style [ ( "visibility", "hidden" ) ]
    ]
    [ section
        [ id "todoapp" ]
        [ lazy2 taskEntry address model.field
        , lazy3 taskList address model.visibility model.todos
        , lazy3 controls address model.visibility model.todos
        ]
    , infoFooter
    ]


onEnter : Address a -> a -> Attribute
onEnter address value =
  on
    "keydown"
    (Json.Decode.customDecoder keyCode is13)
    (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then
    Ok ()
  else
    Err "not the right key code"


taskEntry : Address Action -> String -> Html
taskEntry address task =
  header
    [ id "header" ]
    [ h1 [] [ text "todos" ]
    , input
        [ id "new-todo"
        , placeholder "What needs to be done?"
        , autofocus True
        , value task
        , name "newTodo"
        , on "input" targetValue (Signal.message address << UpdateField)
        , onEnter address Add
        ]
        []
    ]


taskList : Address Action -> String -> List Todo -> Html
taskList address visibility todos =
  let
    isVisible todo =
      case visibility of
        "Completed" ->
          todo.completed

        "Active" ->
          not todo.completed

        _ ->
          True

    allCompleted =
      List.all .completed todos

    cssVisibility =
      if List.isEmpty todos then
        "hidden"
      else
        "visible"
  in
    section
      [ id "main"
      , style [ ( "visibility", cssVisibility ) ]
      ]
      [ input
          [ id "toggle-all"
          , type' "checkbox"
          , name "toggle"
          , checked allCompleted
          , onClick address (CheckAll (not allCompleted))
          ]
          []
      , label
          [ for "toggle-all" ]
          [ text "Mark all as complete" ]
      , ul
          [ id "todo-list" ]
          (List.map (todoItem address) (List.filter isVisible todos))
      ]


todoItem : Address Action -> Todo -> Html
todoItem address todo =
  li
    [ classList [ ( "completed", todo.completed ), ( "editing", todo.editing ) ] ]
    [ div
        [ class "view" ]
        [ input
            [ class "toggle"
            , type' "checkbox"
            , checked todo.completed
            , onClick address (Check todo.id (not todo.completed))
            ]
            []
        , label
            [ onDoubleClick address (EditingTodo todo.id True) ]
            [ text todo.description ]
        , button
            [ class "destroy"
            , onClick address (Delete todo.id)
            ]
            []
        ]
    , input
        [ class "edit"
        , value todo.description
        , name "title"
        , id ("todo-" ++ toString todo.id)
        , on "input" targetValue (Signal.message address << UpdateTodo todo.id)
        , onBlur address (EditingTodo todo.id False)
        , onEnter address (EditingTodo todo.id False)
        ]
        []
    ]


controls : Address Action -> String -> List Todo -> Html
controls address visibility todos =
  let
    tasksCompleted =
      List.length (List.filter .completed todos)

    tasksLeft =
      List.length todos - tasksCompleted

    item_ =
      if tasksLeft == 1 then
        " item"
      else
        " items"
  in
    footer
      [ id "footer"
      , hidden (List.isEmpty todos)
      ]
      [ span
          [ id "todo-count" ]
          [ strong [] [ text (toString tasksLeft) ]
          , text (item_ ++ " left")
          ]
      , ul
          [ id "filters" ]
          [ visibilitySwap address "#/" "All" visibility
          , text " "
          , visibilitySwap address "#/active" "Active" visibility
          , text " "
          , visibilitySwap address "#/completed" "Completed" visibility
          ]
      , button
          [ class "clear-completed"
          , id "clear-completed"
          , hidden (tasksCompleted == 0)
          , onClick address DeleteComplete
          ]
          [ text ("Clear completed (" ++ toString tasksCompleted ++ ")") ]
      ]


visibilitySwap : Address Action -> String -> String -> String -> Html
visibilitySwap address uri visibility actualVisibility =
  li
    [ onClick address (ChangeVisibility visibility) ]
    [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ] [ text visibility ] ]


infoFooter : Html
infoFooter =
  footer
    [ id "info" ]
    [ p [] [ text "Double-click to edit a todo" ]
    , p
        []
        [ text "Written by "
        , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
        ]
    , p
        []
        [ text "Part of "
        , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
        ]
    ]



---- INPUTS ----
-- wire the entire application together


app =
  StartApp.start
    { init = ( emptyModel, Effects.none )
    , view = view
    , update = update
    , inputs = [ actions.signal ]
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


focusIdMailbox : Signal.Mailbox Int
focusIdMailbox =
  Signal.mailbox 0


port focus : Signal String
port focus =
  let
    toSelector id =
      "#todo-" ++ toString id
  in
    focusIdMailbox.signal
      |> Signal.map toSelector


port loadInitialState : Task.Task Http.Error ()
port loadInitialState =
  Http.get todosDecoder "http://serviceworker-todo.herokuapp.com/todos"
    `andThen` (\todos -> Signal.send actions.address <| InitializeState todos)


todoDecoder : Json.Decode.Decoder Todo
todoDecoder =
  Json.Decode.succeed Todo
    |: ("title" := Json.Decode.string)
    |: ("completed" := Json.Decode.bool)
    |: Json.Decode.succeed False
    |: ("order" := Json.Decode.int)


todosDecoder : Json.Decode.Decoder (List Todo)
todosDecoder =
  Json.Decode.list todoDecoder
