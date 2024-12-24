open Aoclib
open Aoclib.Util
open Aoclib.Dir

type xdir = Dir of dir | Omni

module DirectionalPathfind = Astar.Make (struct
  type t = coord * xdir

  let compare = compare

  let to_string (p, dir) =
    Printf.sprintf "(%s %s)" (Coord.to_string p)
      (match dir with Omni -> "Omni" | Dir d -> Dir.to_string d)
end)

let day16a i =
  let lines = In_channel.input_lines i in
  let height = List.length lines in
  let width = List.hd lines |> String.length in
  let table = Hashset.create (height * width) in
  let start = ref (0, 0) in
  let goal = ref (0, 0) in
  let add_row y s =
    List.iteri
      (fun x c ->
        if c = '#' then Hashset.add table (x, y);
        if c = 'S' then start := (x, y);
        if c = 'E' then goal := (x, y))
      (chars s)
  in
  List.iteri (fun y line -> add_row y line) lines;

  let in_bounds (x, y) = x >= 0 && y >= 0 && x < width && y < height in
  let dirs = [ Dir.Up; Dir.Right; Dir.Down; Dir.Left ] in
  let neighbours (p, _) =
    List.filter_map
      (fun ddir ->
        let d = Dir.to_coord ddir in
        let n = Coord.(p + d) in
        if in_bounds n && (not @@ Hashset.mem table n) then
          Some (n, if n = !goal then Omni else Dir ddir)
        else None)
      dirs
  in
  let rotations dir1 dir2 =
    match (dir1, dir2) with
    | _, Omni | Omni, _ -> 0
    | Dir a, Dir b -> (
        match (a, b) with
        | Up, Right
        | Up, Left
        | Right, Up
        | Right, Down
        | Down, Right
        | Down, Left
        | Left, Down
        | Left, Up ->
            1
        | Up, Down | Down, Up | Left, Right | Right, Left -> 2
        | Up, Up | Right, Right | Left, Left | Down, Down -> 0)
  in
  let distance (p1, dir1) (p2, dir2) =
    Coord.manhattan p1 p2 + (rotations dir1 dir2 * 1000)
  in
  let path =
    DirectionalPathfind.pathfind
      (distance (!goal, Omni))
      neighbours distance (!start, Dir Right) (!goal, Omni)
    |> Option.get
  in
  List.fold_left
    (fun (last, cost) next -> (next, cost + distance last next))
    ((!start, Dir Right), 0)
    path
  |> snd

let day16b i =
  ignore i;
  failwith "TODO"
