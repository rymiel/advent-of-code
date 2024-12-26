open Aoclib
open Aoclib.Util

let day10a i =
  let diffs =
    In_channel.input_lines i |> List.map int_of_string |> List.cons 0
    |> List.sort compare |> List.to_seq |> sliding_pair
    |> Seq.map (fun (a, b) -> b - a)
    |> List.of_seq
  in
  let ones = ListExt.count_item 1 diffs in
  let threes = 1 + ListExt.count_item 3 diffs in
  ones * threes

let day10b i =
  let adapters = In_channel.input_lines i |> List.map int_of_string in
  let adapter_set = Hashset.create 1_000_000 in
  Hashset.add adapter_set 0;
  Hashset.add_seq adapter_set (List.to_seq adapters);
  let cache = Hashtbl.create 1_000_000 in
  let rec arrangements target current =
    let aux () =
      if not @@ Hashset.mem adapter_set current then 0
      else
        ListExt.map_sum
          (fun n -> arrangements target n)
          [ current + 1; current + 2; current + 3 ]
    in
    if current = target then 1
    else
      match Hashtbl.find_opt cache current with
      | Some x -> x
      | None ->
          let r = aux () in
          Hashtbl.replace cache current r;
          r
  in
  arrangements (ListExt.max adapters) 0
