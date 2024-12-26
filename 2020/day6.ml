open Aoclib.Util

let day6a i =
  In_channel.input_lines i |> partition_all ""
  |> List.map (fun lines ->
         lines |> String.concat "" |> chars |> List.sort_uniq compare
         |> List.length)
  |> sum

module CharSet = Set.Make (Char)

let day6b i =
  let intersect_all = function
    | [] -> CharSet.empty
    | x :: xs -> List.fold_left CharSet.inter x xs
  in
  In_channel.input_lines i |> partition_all ""
  |> List.map (fun lines ->
         lines
         |> List.map (fun line -> line |> chars |> CharSet.of_list)
         |> intersect_all |> CharSet.cardinal)
  |> sum
