open Aoclib.Util

let day1a i =
  In_channel.input_lines i |> partition_all ""
  |> List.map (ListExt.map_sum int_of_string)
  |> ListExt.max

let day1b i =
  In_channel.input_lines i |> partition_all ""
  |> List.map (ListExt.map_sum int_of_string)
  |> List.sort compare |> List.rev |> List.to_seq |> Seq.take 3 |> SeqExt.sum
