open Aoclib.Util

let parse i =
  In_channel.input_all i |> String.trim |> String.split_on_char ','
  |> List.map int_of_string

let day7a i =
  let nums = parse i in
  let cost target = ListExt.map_sum (fun a -> abs (target - a)) nums in
  List.sort_uniq compare nums |> List.map cost |> ListExt.min

let day7b i =
  let nums = parse i in
  let triangle n = ((n * n) + n) / 2 in
  let cost target =
    ListExt.map_sum (fun a -> triangle @@ abs (target - a)) nums
  in
  range (ListExt.min nums) (ListExt.max nums) |> Seq.map cost |> SeqExt.min
