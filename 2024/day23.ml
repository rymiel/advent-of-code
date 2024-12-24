open Aoclib
open Aoclib.Util

let parse_day23 i =
  let nodes =
    In_channel.input_lines i
    |> List.map (String.split_on_char '-')
    |> List.map as_pair
  in
  let graph = Hashtbl.create 1000 in
  List.iter
    (fun (a, b) ->
      Hashtbl.add graph a b;
      Hashtbl.add graph b a)
    nodes;
  (nodes, graph)

let day23a i =
  let nodes, graph = parse_day23 i in
  let triplets = Hashset.create 1000 in
  List.iter
    (fun (k, v) ->
      let kset = Hashtbl.find_all graph k |> StringSet.of_list in
      let vset = Hashtbl.find_all graph v |> StringSet.of_list in
      let inter = StringSet.inter kset vset in
      StringSet.iter
        (fun c -> Hashset.add triplets (List.sort String.compare [ k; v; c ]))
        inter)
    nodes;
  Hashset.to_seq triplets
  |> Seq.filter (fun triplet -> triplet |> List.exists (fun s -> s.[0] = 't'))
  |> Seq.length

let day23b i =
  let nodes, graph = parse_day23 i in
  let intersets = Hashset.create 1_000_000 in
  let rec common_intersections points =
    let key = List.sort String.compare points |> String.concat "," in
    if Hashset.mem intersets key then ()
    else (
      Hashset.add intersets key;
      let sets =
        List.map (fun p -> Hashtbl.find_all graph p |> StringSet.of_list) points
      in
      List.fold_left (fun acc s -> StringSet.inter acc s) (List.hd sets) sets
      |> StringSet.iter (fun common -> common_intersections (common :: points)))
  in
  List.iter (fun (k, v) -> common_intersections [ k; v ]) nodes;
  intersets |> Hashset.to_seq
  |> Seq.fold_left
       (fun best i -> if String.length i > String.length best then i else best)
       ""
  |> print_endline;
  0
