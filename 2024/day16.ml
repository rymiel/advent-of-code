open Aoclib
open Aoclib.Util
open Aoclib.Dir

type xdir = Dir of dir | Omni

module DirectionalPathfind = Astar.Make (struct
  type t = coord * xdir

  let compare = compare
end)

let parse i =
  read_maze i (fun { start; goal } pos c ->
      if c = 'S' then start := pos;
      if c = 'E' then goal := pos;
      if c = '#' then Some () else None)

let dirs = [ Up; Right; Down; Left ]
let in_bounds width height (x, y) = x >= 0 && y >= 0 && x < width && y < height

let neighbours width height table goal (p, xdir) =
  let in_bounds = in_bounds width height in
  dirs
  |> List.filter (fun ddir ->
         match xdir with
         | Omni -> true
         | Dir dir ->
             dir = ddir || Dir.rotate_cw dir = ddir || Dir.rotate_ccw dir = ddir)
  |> List.filter_map (fun ddir ->
         let d = Dir.to_coord ddir in
         let n = Coord.(p + d) in
         if in_bounds n && (not @@ Hashset.mem table n) then
           Some (n, if n = goal then Omni else Dir ddir)
         else None)

let rotations dir1 dir2 =
  match (dir1, dir2) with
  | _, Omni | Omni, _ -> 0
  | Dir a, Dir b -> if a = b then 0 else 1

let distance (p1, dir1) (p2, dir2) =
  Coord.manhattan p1 p2 + (rotations dir1 dir2 * 1000)

let day16a i =
  let { Maze.table; width; height; start; goal } = parse i in
  let neighbours = neighbours width height table goal in
  let path =
    DirectionalPathfind.pathfind
      (distance (goal, Omni))
      neighbours distance (start, Dir Right) (goal, Omni)
    |> Option.get
  in
  List.fold_left
    (fun (last, cost) next -> (next, cost + distance last next))
    ((start, Dir Right), 0)
    path
  |> snd

let day16b i =
  let { Maze.table; width; height; start; goal } = parse i in
  let neighbours = neighbours width height table goal in
  let set = Hashset.create 1_000_000 in
  let reconstruct_paths came_from current =
    let rec aux current =
      Hashset.add set (fst current);
      List.iter aux (Hashtbl.find_all came_from current)
    in
    aux current
  in
  let modified_pathfind () =
    let came_from = Hashtbl.create 100 in
    let cost_so_far = Hashtbl.create 100 in
    Hashtbl.add cost_so_far (start, Dir Right) 0;
    let frontier = Queue.create () in
    Queue.add (start, Dir Right) frontier;
    let rec loop () =
      match Queue.take_opt frontier with
      | None -> reconstruct_paths came_from (goal, Omni)
      | Some current ->
          if current <> (goal, Omni) then
            List.iter
              (fun next ->
                let new_cost =
                  Hashtbl.find cost_so_far current + distance current next
                in
                let g_next =
                  Hashtbl.find_opt cost_so_far next
                  |> Option.value ~default:Int.max_int
                in
                if new_cost = g_next then Hashtbl.add came_from next current
                else if new_cost < g_next then (
                  Hashtbl.replace came_from next current;
                  Hashtbl.replace cost_so_far next new_cost;
                  Queue.add next frontier))
              (neighbours current);
          loop ()
    in
    loop ()
  in
  modified_pathfind ();
  Hashset.length set
