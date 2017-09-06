module List.Extra exposing (dropLast, append)

dropLast list =
  List.reverse list |> List.drop 1 |> List.reverse

append list object =
  List.append list <| List.singleton object
