open Aoclib.Util

let day1a i =
  In_channel.input_lines i |> List.map int_of_string |> List.to_seq
  |> sliding_pair
  |> Seq.map (fun (a, b) -> b - a)
  |> SeqExt.count_matches (fun i -> i > 0)

let day1b i =
  In_channel.input_lines i |> List.map int_of_string |> List.to_seq
  |> sliding_window 3 |> Seq.map ArrayExt.sum |> sliding_pair
  |> Seq.map (fun (a, b) -> b - a)
  |> SeqExt.count_matches (fun i -> i > 0)
