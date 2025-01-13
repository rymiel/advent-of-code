open Aoclib
open Aoclib.Util

let day8a i =
  In_channel.input_lines i
  |> List.concat_map (fun line ->
         line |> String.split_on_char '|' |> as_pair |> snd |> String.trim
         |> String.split_on_char ' ' |> List.map String.length)
  |> ListExt.count_matches (fun i -> List.mem i [ 2; 3; 4; 7 ])

module CharSet = Set.Make (Char)

let solve line =
  let input, output =
    String.split_on_char '|' line
    |> as_pair
    |> Pair.map (fun s ->
           String.trim s |> String.split_on_char ' '
           |> List.map (fun i -> String.to_seq i |> CharSet.of_seq))
  in
  let one = List.find (fun s -> CharSet.cardinal s = 2) input in
  let four = List.find (fun s -> CharSet.cardinal s = 4) input in
  let nums =
    List.map
      (fun set ->
        match CharSet.cardinal set with
        | 2 -> 1
        | 3 -> 7
        | 4 -> 4
        | 5 ->
            if CharSet.subset one set then 3
            else if CharSet.inter four set |> CharSet.cardinal = 3 then 5
            else 2
        | 6 ->
            if CharSet.subset four set then 9
            else if CharSet.subset one set then 0
            else 6
        | 7 -> 8
        | _ -> failwith "invalid")
      output
  in
  List.map string_of_int nums |> String.concat "" |> int_of_string

let day8b i =
  let input = In_channel.input_lines i in
  ListExt.map_sum solve input
