open Aoclib.Util
open Aoclib

let split s =
  let half = String.length s / 2 in
  (String.sub s 0 half, String.sub s half half)

let priority c =
  match c with
  | 'a' .. 'z' -> int_of_char c - int_of_char 'a' + 1
  | 'A' .. 'Z' -> int_of_char c - int_of_char 'A' + 27
  | _ -> invalid_arg "priority"

module CharSet = Set.Make (Char)

let day3a i =
  In_channel.input_lines i
  |> ListExt.map_sum (fun s ->
         let left, right =
           split s |> Pair.map (Fun.compose CharSet.of_list chars)
         in
         CharSet.inter left right |> CharSet.to_list |> List.map priority |> sum)

let day3b i =
  In_channel.input_lines i |> List.to_seq |> fixed_window 3
  |> SeqExt.map_sum (fun f ->
         f
         |> Array.map (Fun.compose CharSet.of_list chars)
         |> ArrayExt.fold_left' CharSet.inter
         |> CharSet.to_list |> ListExt.map_sum priority)
