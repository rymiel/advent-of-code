open Aoclib
open Aoclib.Util

let parse i =
  let lines = In_channel.input_lines i in
  let height = List.length lines in
  let width = List.hd lines |> String.length in
  let table = Hashtbl.create (height * width) in
  let add_row y s =
    List.iteri
      (fun x c -> Hashtbl.add table (x, y) c)
      (chars_s s |> List.map int_of_string)
  in
  List.iteri (fun y line -> add_row y line) lines;
  table

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
