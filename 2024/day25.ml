open Aoclib.Util

let partition_locks_keys = function
  | [] -> failwith "empty"
  | head :: tail ->
      let is_lock = head = "#####" in
      let columns =
        List.to_seq tail |> Seq.take 5 |> Seq.map String.to_seq |> Seq.transpose
        |> Seq.map (seq_count_item '#')
        |> List.of_seq
      in
      (if is_lock then Either.left else Either.right) columns

let fits = List.for_all2 (fun a b -> a + b <= 5)

let day25a i =
  let locks, keys =
    i |> In_channel.input_lines |> partition_all ""
    |> List.partition_map partition_locks_keys
  in
  List.map (fun key -> count_matches (fits key) locks) keys |> sum
