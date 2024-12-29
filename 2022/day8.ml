open Aoclib
open Aoclib.Util

let parse i = (read_coord_table i (fun c -> Some (digit_of_char c))).table
let dirs = [ Dir.Up; Dir.Right; Dir.Down; Dir.Left ]

let day8a i =
  let table = parse i in
  let rec raycast num pos dir =
    let n = Coord.(pos >> dir) in
    match Hashtbl.find_opt table n with
    | None -> true
    | Some x -> x < num && raycast num n dir
  in
  let visible pos = List.exists (raycast (Hashtbl.find table pos) pos) dirs in
  Hashtbl.to_seq_keys table |> SeqExt.count_matches visible

let day8b i =
  let table = parse i in
  let rec visible_trees num pos dir =
    let n = Coord.(pos >> dir) in
    match Hashtbl.find_opt table n with
    | None -> 0
    | Some x -> 1 + if x >= num then 0 else visible_trees num n dir
  in
  let scenic_score pos =
    dirs
    |> List.map (visible_trees (Hashtbl.find table pos) pos)
    |> ListExt.product
  in
  Hashtbl.to_seq_keys table |> Seq.map scenic_score |> SeqExt.max
