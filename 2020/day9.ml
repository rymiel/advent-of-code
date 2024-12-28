open Aoclib.Util

let part1 nums =
  let window_size = 25 in
  nums |> List.to_seq
  |> Seq.scan
       (fun (p, arr) n -> (n, array_shift_left arr p))
       (0, Array.make window_size 0)
  |> Seq.drop (window_size + 1)
  |> Seq.find (fun (i, arr) ->
         arr |> Array.to_seq |> permute_pairs
         |> Seq.exists (fun (a, b) -> a + b = i)
         |> not)
  |> Option.get |> fst

let day9a i = In_channel.input_lines i |> List.map int_of_string |> part1

let day9b i =
  let nums = In_channel.input_lines i |> List.map int_of_string in
  let invalid = part1 nums in
  Seq.ints 2
  |> Seq.find_map (fun size ->
         nums |> List.to_seq |> sliding_window size
         |> Seq.find (fun arr -> ArrayExt.sum arr = invalid))
  |> Option.get
  |> fun arr -> ArrayExt.min arr + ArrayExt.max arr
