open Aoclib
open Aoclib.Util

let parse i =
  let lines = In_channel.input_lines i in
  let height = List.length lines in
  let width = List.hd lines |> String.length in
  let table = Hashtbl.create (height * width) in
  let start = ref (0, 0) in
  let goal = ref (0, 0) in
  let add_row y s =
    Seq.iteri
      (fun x c ->
        Hashtbl.add table (x, y)
          (match c with
          | 'S' ->
              start := (x, y);
              int_of_char 'a'
          | 'E' ->
              goal := (x, y);
              int_of_char 'z'
          | c -> int_of_char c))
      (String.to_seq s)
  in
  List.iteri (fun y line -> add_row y line) lines;
  (table, width, height, !start, !goal)

let elevation_pathfind table width height start goal =
  let in_bounds (x, y) = x >= 0 && y >= 0 && x < width && y < height in
  let dirs = [ Dir.Up; Dir.Right; Dir.Down; Dir.Left ] in
  let valid_step f t = Hashtbl.find table f + 1 >= Hashtbl.find table t in
  let neighbours p =
    List.map (fun d -> Coord.(p >> d)) dirs
    |> List.filter (fun n -> in_bounds n && valid_step p n)
  in
  Pathfind.pathfind (Coord.manhattan goal) neighbours Coord.manhattan start goal
  |> Option.fold ~some:(fun ls -> List.length ls - 1) ~none:Int.max_int

let day12a i =
  let table, width, height, start, goal = parse i in
  elevation_pathfind table width height start goal

let day12b i =
  let table, width, height, _, goal = parse i in
  Hashtbl.to_seq table
  |> Seq.filter (fun (_, i) -> i = int_of_char 'a')
  |> Seq.map fst
  |> Seq.map (fun start -> elevation_pathfind table width height start goal)
  |> SeqExt.min
