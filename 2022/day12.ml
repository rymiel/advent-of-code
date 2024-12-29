open Aoclib
open Aoclib.Util

let parse i =
  read_maze i (fun { start; goal } pos c ->
      Some
        (match c with
        | 'S' ->
            start := pos;
            int_of_char 'a'
        | 'E' ->
            goal := pos;
            int_of_char 'z'
        | c -> int_of_char c))

let elevation_pathfind { Maze.table; width; height; start; goal } =
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
  let maze = parse i in
  elevation_pathfind maze

let day12b i =
  let maze = parse i in
  Hashtbl.to_seq maze.table
  |> Seq.filter (fun (_, i) -> i = int_of_char 'a')
  |> Seq.map fst
  |> Seq.map (fun start -> elevation_pathfind { maze with start })
  |> SeqExt.min
