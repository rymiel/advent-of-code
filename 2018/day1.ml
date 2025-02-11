open Aoclib
open Aoclib.Util

let part1 i = In_channel.input_lines i |> ListExt.map_sum int_of_string

let part2 i =
  let cache = Hashset.create 1000 in
  In_channel.input_lines i |> List.to_seq |> Seq.map int_of_string |> Seq.cycle
  |> Seq.scan ( + ) 0
  |> Seq.find (fun i ->
         if Hashset.mem cache i then true
         else (
           Hashset.add cache i;
           false))
  |> Option.get
