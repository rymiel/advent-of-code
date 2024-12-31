open Aoclib
open Aoclib.Util

let in_range i (_, (a, b), (c, d)) = (i >= a && i <= b) || (i >= c && i <= d)

let parse i =
  let fields, tickets = In_channel.input_lines i |> partition_at "" in
  let fields =
    List.map
      (fun s ->
        Scanf.sscanf s "%s@: %d-%d or %d-%d" (fun n a b c d ->
            (n, (a, b), (c, d))))
      fields
  in
  let my_ticket, nearby =
    partition_at "" tickets
    |> Pair.map (fun l ->
           List.tl l
           |> List.map (fun s ->
                  String.split_on_char ',' s |> List.map int_of_string))
  in
  let my_ticket = List.hd my_ticket in
  (fields, my_ticket, nearby)

let day16a i =
  let fields, _, nearby = parse i in
  let error_rate =
    ListExt.map_sum (fun i -> if List.exists (in_range i) fields then 0 else i)
  in
  ListExt.map_sum error_rate nearby

let day16b i =
  let fields, my_ticket, nearby = parse i in
  let valid ticket =
    List.for_all (fun i -> List.exists (in_range i) fields) ticket
  in
  let nearby = List.filter valid nearby in
  let columns =
    List.to_seq nearby |> Seq.map List.to_seq |> Seq.transpose
    |> Seq.map List.of_seq |> List.of_seq
  in
  let valid_fields i =
    List.filter (in_range i) fields |> List.map (fun (n, _, _) -> n)
  in
  let intersect_fields is =
    List.map (Fun.compose StringSet.of_list valid_fields) is
    |> ListExt.fold_left' StringSet.inter
  in
  let rec resolve = function
    | [] -> []
    | fields ->
        let up, rest =
          List.partition (fun (_, s) -> StringSet.cardinal s = 1) fields
        in
        let res = List.map (fun (i, s) -> (i, StringSet.min_elt s)) up in
        let skip s =
          List.fold_left (fun s (_, e) -> StringSet.remove e s) s res
        in
        let rest = (List.map (fun (i, s) -> (i, skip s))) rest in
        res @ resolve rest
  in
  resolve (List.mapi (fun i c -> (i, intersect_fields c)) columns)
  |> List.filter (fun (_, a) -> String.starts_with ~prefix:"departure" a)
  |> List.map (fun (i, _) -> List.nth my_ticket i)
  |> ListExt.product
