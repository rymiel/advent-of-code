open Aoclib
open Aoclib.Util

let parse i =
  let template, rule_lines = In_channel.input_lines i |> partition_at "" in
  let template = List.hd template in
  let rules = Hashtbl.create 1000 in
  List.iter
    (fun line ->
      Scanf.sscanf line "%c%c -> %c" (fun k1 k2 v ->
          Hashtbl.add rules (k1, k2) ((k1, v), (v, k2))))
    rule_lines;
  (template, rules)

let day14a i =
  let template, rules = parse i in
  let do_step = Seq.flat_map (fun p -> Hashtbl.find rules p |> Pair.to_seq) in
  let destagger seq =
    let (a, b), tail = Option.get (Seq.uncons seq) in
    Seq.append (List.to_seq [ a; b ]) (Seq.map snd tail)
  in
  let score seq =
    let tally = SeqExt.tally (destagger seq) in
    let lowest, highest =
      SeqExt.map_minmax Fun.id (Hashtbl.to_seq_values tally)
    in
    highest - lowest
  in
  let seq = template |> String.to_seq |> sliding_pair in
  Seq.iterate do_step seq |> Seq.drop 10 |> head |> score

module CharPairMap = Map.Make (struct
  type t = char * char

  let compare = compare
end)

module CharMap = Map.Make (Char)

let day14b i =
  let template, rules = parse i in
  let first = template.[0] in
  let last = template.[String.length template - 1] in
  let add v = function Some x -> Some (x + v) | None -> Some v in
  let initial =
    String.to_seq template |> sliding_pair
    |> Seq.fold_left
         (fun map k -> CharPairMap.update k (add 1) map)
         CharPairMap.empty
  in
  let do_step map =
    CharPairMap.to_seq map
    |> Seq.flat_map (fun (k, v) ->
           Hashtbl.find rules k |> Pair.map (fun a -> (a, v)) |> Pair.to_seq)
    |> Seq.fold_left
         (fun map (k, v) -> CharPairMap.update k (add v) map)
         CharPairMap.empty
  in
  let score map =
    let tally =
      CharPairMap.to_seq map
      |> Seq.flat_map (fun (k, v) ->
             k |> Pair.map (fun a -> (a, v)) |> Pair.to_seq)
      |> Seq.fold_left
           (fun map (k, v) -> CharMap.update k (add v) map)
           CharMap.empty
      |> CharMap.update first (add 1)
      |> CharMap.update last (add 1)
    in
    let lowest, highest = SeqExt.map_minmax snd (CharMap.to_seq tally) in
    (highest - lowest) / 2
  in
  Seq.iterate do_step initial |> Seq.drop 40 |> head |> score
