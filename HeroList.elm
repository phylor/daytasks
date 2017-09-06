module HeroList exposing (HeroList, singleton, previousOrCreate, nextOrCreate)

import List.Extra exposing (..)

type alias HeroList a =
  { before : List a
  , current : a
  , after : List a
  }

singleton : a -> HeroList a
singleton object =
  HeroList [] object []

previousOrCreate : HeroList a -> a -> HeroList a
previousOrCreate list newObject =
  if List.length list.before == 0 then
    HeroList [] newObject (list.current :: list.after)
  else
    previous list

previous : HeroList a -> HeroList a
previous list =
  case List.head <| List.reverse list.before of
    Just newCurrent ->
      let
        newBefore = dropLast list.before
      in
        HeroList newBefore newCurrent (list.current :: list.before)
    Nothing ->
      list

nextOrCreate : HeroList a -> a -> HeroList a
nextOrCreate list newObject =
  if List.length list.after == 0 then
    HeroList (append list.before list.current) newObject []
  else
    next list

next : HeroList a -> HeroList a
next list =
  case List.head list.after of
    Just newCurrent ->
      HeroList
        (append list.before newCurrent)
        newCurrent
        (List.drop 1 list.after)
    Nothing ->
      list
