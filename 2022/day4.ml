open Aoclib.Util
open Aoclib

let read_range s =
  String.split_on_char '-' s |> as_pair |> Pair.map int_of_string

let read_range_pair s =
  String.split_on_char ',' s |> as_pair |> Pair.map read_range

let contains (a1, a2) (b1, b2) = a1 <= b1 && a2 >= b2

let overlaps (a1, a2) (b1, b2) =
  (a2 >= b1 && not (a2 > b2)) || (b2 >= a1 && not (b2 > a2))

let day4a i =
  In_channel.input_lines i |> List.map read_range_pair
  |> ListExt.count_matches (fun (left, right) ->
         contains left right || contains right left)

let day4b i =
  In_channel.input_lines i |> List.map read_range_pair
  |> ListExt.count_matches (fun (left, right) -> overlaps left right)
