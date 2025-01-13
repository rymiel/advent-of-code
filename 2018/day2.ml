open Aoclib.Util

let part1 i =
  let input =
    In_channel.input_lines i
    |> List.map (fun s -> String.to_seq s |> SeqExt.tally)
  in
  let count_with_occurences n =
    ListExt.count_matches
      (fun table -> Hashtbl.to_seq_values table |> Seq.exists (( = ) n))
      input
  in
  count_with_occurences 2 * count_with_occurences 3

let part2 i =
  let input =
    In_channel.input_lines i |> List.map String.to_seq |> List.to_seq
  in
  let off_by = Seq.fold_left2 (fun i a b -> if a = b then i else i + 1) 0 in
  let common (a, b) =
    Seq.map2 (fun a b -> if a = b then a else '\000') a b
    |> Seq.filter (( <> ) '\000')
    |> String.of_seq
  in
  permute_pairs input
  |> Seq.filter (fun (a, b) -> off_by a b = 1)
  |> head |> common |> print_endline;
  0
