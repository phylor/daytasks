import Html exposing (program, div, text, button)
import Html.Events exposing (onClick)
import Date exposing (Date)
import Task
import HeroList exposing (HeroList)
import Date.Extra.Duration

type Model
  = Loading
  | LoadedModel DayTaskModel

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

init =
  ( Loading, createTodayDayTask )

createTodayDayTask =
  Task.perform CreateTodayDayTask Date.now

update message model =
  case model of
    Loading ->
      case message of
        CreateTodayDayTask date ->
          ( LoadedModel <| HeroList.singleton (DayTask date []), Cmd.none )
        PreviousDay ->
          ( model, Cmd.none )
        NextDay ->
          ( model, Cmd.none )
    LoadedModel list ->
      case message of
        CreateTodayDayTask _ ->
          ( model, Cmd.none )
        PreviousDay ->
          let
            previousDay = Date.Extra.Duration.add Date.Extra.Duration.Day -1 list.current.day
          in
            ( LoadedModel <| HeroList.previousOrCreate list (DayTask previousDay []), Cmd.none )
        NextDay ->
          let
            nextDay = Date.Extra.Duration.add Date.Extra.Duration.Day 1 list.current.day
          in
            ( LoadedModel <| HeroList.nextOrCreate list (DayTask nextDay []), Cmd.none )

view model =
  case model of
    Loading ->
      div [] [ text "Loading" ]

    LoadedModel model ->
      div []
        [ button [ onClick PreviousDay ] [ text "<" ]
        , div [] [ text <| toString model.current.day ]
        , button [ onClick NextDay ] [ text ">" ]
        ]
