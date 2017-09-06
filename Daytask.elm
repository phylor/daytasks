import Html exposing (program, div, text, button, input, Attribute)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Json.Decode
import Date exposing (Date)
import Task
import HeroList exposing (HeroList)
import Date.Extra.Duration

type Model
  = Loading
  | LoadedModel (WebModel TemporaryModel DayTaskModel)

type alias WebModel a b =
  { temporary : a
  , dirty : b
  , persisted : b
  }

type alias TemporaryModel =
  { newTask : String
  }

type alias DayTaskModel =
  HeroList DayTask

type alias DayTask =
  { day : Date
  , tasks : List Task
  }

type alias Task =
  { title : String
  , start : Date
  , end : Date
  }

main = Html.program
  { init = init
  , update = update
  , subscriptions = \_ -> Sub.none
  , view = view
  }

type Message
  = CreateTodayDayTask Date
  | PreviousDay
  | NextDay
  | InputNewTask String
  | KeyDown Int

init =
  ( Loading, createTodayDayTask )

createTodayDayTask =
  Task.perform CreateTodayDayTask Date.now

update message model =
  case model of
    Loading ->
      case message of
        CreateTodayDayTask date ->
          let
            newHeroList = HeroList.singleton (DayTask date [])
          in
            ( LoadedModel <| WebModel (TemporaryModel "") newHeroList newHeroList, Cmd.none )
        _ ->
          ( model, Cmd.none )
    LoadedModel model ->
      case message of
        CreateTodayDayTask _ ->
          ( LoadedModel model, Cmd.none )
        PreviousDay ->
          let
            previousDay = Date.Extra.Duration.add Date.Extra.Duration.Day -1 model.dirty.current.day
          in
            ( LoadedModel <| WebModel model.temporary (HeroList.previousOrCreate model.dirty (DayTask previousDay [])) model.persisted, Cmd.none )
        NextDay ->
          let
            nextDay = Date.Extra.Duration.add Date.Extra.Duration.Day 1 model.dirty.current.day
          in
            ( LoadedModel <| WebModel model.temporary (HeroList.nextOrCreate model.dirty (DayTask nextDay [])) model.persisted, Cmd.none )
        InputNewTask text ->
          let
            temporaryModel = model.temporary
            newTemporaryModel = { temporaryModel | newTask = text }
          in
            ( LoadedModel { model | temporary = newTemporaryModel }, Cmd.none )
        KeyDown 13 ->
          let
            newTaskTitle = model.temporary.newTask
            temporaryModel = model.temporary
            newTemporaryModel = { temporaryModel | newTask = "" }
            newTask = Task newTaskTitle model.dirty.current.day model.dirty.current.day
            dayModel = model.dirty.current
            newDayModel = { dayModel | tasks = newTask :: dayModel.tasks }
            dirty = model.dirty
            newDirty = { dirty | current = newDayModel }
          in
          -- save task: add DayTask for the current day
            ( LoadedModel { model | temporary = newTemporaryModel, dirty = newDirty }, Cmd.none )
        KeyDown _ ->
          ( LoadedModel model, Cmd.none )

view model =
  case model of
    Loading ->
      div [] [ text "Loading" ]

    LoadedModel model ->
      div []
        [ button [ onClick PreviousDay ] [ text "<" ]
        , div [] [ text <| toString model.dirty.current.day ]
        , button [ onClick NextDay ] [ text ">" ]
        , div []
            (List.map renderTask model.dirty.current.tasks)
        , input [ onInput InputNewTask, onKeyDown KeyDown, value model.temporary.newTask ] []
        ]

onKeyDown : (Int -> Message) -> Attribute Message
onKeyDown tagger =
  on "keydown" (Json.Decode.map tagger keyCode)

renderTask task =
  div [] [ text task.title ]
