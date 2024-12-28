open Aoclib
open Aoclib.Util

let day6 n i =
  (In_channel.input_all i |> String.to_seq |> sliding_window n
  |> Seq.find_index (fun window ->
         Array.to_seq window |> permute_pairs |> Seq.for_all (Pair.apply ( <> )))
  |> Option.get)
  + n

let day6a = day6 4
let day6b = day6 14
