open Aoclib.Util

let rec read_keys_locks (lines : string list) =
  match partition_at "" lines with
  | [], _ -> []
  | head :: tail, rest ->
      let is_lock = head = "#####" in
      let columns =
        List.to_seq tail |> Seq.take 5 |> Seq.map String.to_seq |> Seq.transpose
        |> Seq.map (seq_count_item '#')
        |> List.of_seq
      in
      (if is_lock then Either.left else Either.right) columns
      :: read_keys_locks rest

let fits = List.for_all2 (fun a b -> a + b <= 5)

let day25a i =
  let locks, keys =
    read_keys_locks (In_channel.input_lines i) |> List.partition_map Fun.id
  in
  List.map (fun key -> count_matches (fits key) locks) keys |> sum
