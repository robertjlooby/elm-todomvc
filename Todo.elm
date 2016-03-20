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
  , visibility : String
  }


type alias Todo =
  { description : String
  , completed : Bool
  , editing : Bool
  , uid : String
  }


newTodo : String -> String -> Todo
newTodo desc uid =
  { description = desc
  , completed = False
  , editing = False
  , uid = uid
  }


emptyModel : Model
emptyModel =
  { todos = []
  , visibility = "All"
  , field = ""
  }



---- UPDATE ----
-- A description of the kinds of actions that can be performed on the model of
-- our application. See the following for more info on this pattern and
-- some alternatives: https://github.com/evancz/elm-architecture-tutorial/


type Action
  = NoOp
  | UpdateField String
  | BeginEditingTodo String
  | UpdateTodo String String
  | HandleUpdatedTodo (Maybe Todo)
  | Add
  | HandleNew (Maybe Todo)
  | Delete String
  | HandleDeleted (Maybe String)
  | Check String Bool
  | ChangeVisibility String
  | InitializeState (List Todo)



-- How we update our Model on a given Action?


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    Add ->
      let
        effect =
          if String.isEmpty model.field then
            Effects.none
          else
            Http.post todoDecoder url (Http.string <| "{\"title\":\"" ++ model.field ++ "\"}")
              |> Task.toMaybe
              |> Task.map HandleNew
              |> Effects.task
      in
        ( { model | field = "" }, effect )

    HandleNew todo ->
      case todo of
        Just t ->
          ( { model | todos = model.todos ++ [ t ] }, Effects.none )

        Nothing ->
          ( model, Effects.none )

    UpdateField str ->
      ( { model | field = str }, Effects.none )

    BeginEditingTodo uid ->
      let
        updateTodo t =
          if t.uid == uid then
            { t | editing = True }
          else
            t

        effect =
          Signal.send focusIdMailbox.address uid
            |> Task.map (always NoOp)
            |> Effects.task
      in
        ( { model | todos = List.map updateTodo model.todos }, effect )

    UpdateTodo uid newDescription ->
      case List.filter (\t -> t.uid == uid) model.todos |> List.head of
        Nothing ->
          ( model, Effects.none )

        Just todo ->
          if todo.editing == False then
            ( model, Effects.none )
          else
            let
              updateTodo t =
                if t.uid == uid then
                  { t | editing = False }
                else
                  t

              effect =
                Http.send
                  Http.defaultSettings
                  { verb = "PATCH"
                  , headers = []
                  , url = url ++ "/" ++ uid
                  , body = Http.string <| "{\"title\":\"" ++ newDescription ++ "\"}"
                  }
                  |> Http.fromJson todoDecoder
                  |> Task.toMaybe
                  |> Task.map HandleUpdatedTodo
                  |> Effects.task
            in
              ( { model | todos = List.map updateTodo model.todos }, effect )

    HandleUpdatedTodo maybeTodo ->
      case maybeTodo of
        Just todo ->
          let
            updateTodo t =
              if t.uid == todo.uid then
                todo
              else
                t
          in
            ( { model | todos = List.map updateTodo model.todos }, Effects.none )

        Nothing ->
          ( model, Effects.none )

    Delete uid ->
      let
        handleResponse response =
          case response.status of
            204 ->
              Task.succeed uid

            _ ->
              Task.fail Http.RawNetworkError

        effect =
          Http.send
            Http.defaultSettings
            { verb = "DELETE"
            , headers = []
            , url = url ++ "/" ++ uid
            , body = Http.empty
            }
            `andThen` handleResponse
            |> Task.toMaybe
            |> Task.map HandleDeleted
            |> Effects.task
      in
        ( model, effect )

    HandleDeleted maybeUid ->
      case maybeUid of
        Just uid ->
          ( { model | todos = List.filter (\t -> t.uid /= uid) model.todos }, Effects.none )

        Nothing ->
          ( model, Effects.none )

    Check uid isCompleted ->
      let
        updateTodo t =
          if t.uid == uid then
            { t | completed = isCompleted }
          else
            t
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


keycodeWithTargetValue : Json.Decode.Decoder ( Int, String )
keycodeWithTargetValue =
  Json.Decode.object2
    (,)
    keyCode
    targetValue


targetValueOnEnter : Json.Decode.Decoder String
targetValueOnEnter =
  Json.Decode.customDecoder
    keycodeWithTargetValue
    (\( key, val ) -> is13 key |> Result.map (\_ -> val))


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
      [ ul
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
            , onClick address (Check todo.uid (not todo.completed))
            ]
            []
        , label
            [ onDoubleClick address (BeginEditingTodo todo.uid) ]
            [ text todo.description ]
        , button
            [ class "destroy"
            , onClick address (Delete todo.uid)
            ]
            []
        ]
    , input
        [ class "edit"
        , value todo.description
        , name "title"
        , id ("todo-" ++ todo.uid)
        , on "blur" targetValue (Signal.message address << UpdateTodo todo.uid)
        , on
            "keydown"
            targetValueOnEnter
            (Signal.message address << UpdateTodo todo.uid)
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
        [ a [ href "https://github.com/robertjlooby/elm-todomvc" ] [ text "Source" ]
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


focusIdMailbox : Signal.Mailbox String
focusIdMailbox =
  Signal.mailbox ""


port focus : Signal String
port focus =
  let
    toSelector uid =
      "#todo-" ++ uid
  in
    focusIdMailbox.signal
      |> Signal.map toSelector


url : String
url =
  "http://serviceworker-todo.herokuapp.com/todos"


port loadInitialState : Task.Task Http.Error ()
port loadInitialState =
  Http.get todosDecoder url
    `andThen` (\todos -> Signal.send actions.address <| InitializeState todos)


todoDecoder : Json.Decode.Decoder Todo
todoDecoder =
  Json.Decode.succeed Todo
    |: ("title" := Json.Decode.string)
    |: ("completed" := Json.Decode.bool)
    |: Json.Decode.succeed False
    |: ("uid" := Json.Decode.string)


todosDecoder : Json.Decode.Decoder (List Todo)
todosDecoder =
  Json.Decode.list todoDecoder
