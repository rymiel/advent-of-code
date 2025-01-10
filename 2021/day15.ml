open Aoclib
open Aoclib.Util

let solve { table; width; height } =
  let dirs = [ Coord.up; Coord.right; Coord.down; Coord.left ] in
  let neighbours p =
    dirs
    |> List.map (fun d -> Coord.(p + d))
    |> List.filter (fun n -> Hashtbl.mem table n)
  in
  let start = (0, 0) in
  let goal = (width - 1, height - 1) in
  let heuristic = Coord.manhattan goal in
  let distance _a b = Hashtbl.find table b in
  let path = Pathfind.pathfind heuristic neighbours distance start goal in
  Option.get path |> List.to_seq |> sliding_pair
  |> SeqExt.map_sum (fun (a, b) -> distance a b)

let parse i = read_coord_table i (fun c -> digit_of_char c |> Option.some)

let expand factor { table; width; height } =
  let new_table = Hashtbl.create (Hashtbl.length table * factor * factor) in
  let range = Seq.ints 0 |> Seq.take factor in
  Seq.product range range
  |> Seq.iter (fun (fx, fy) ->
         let d = fx + fy in
         let o = (fx * width, fy * height) in
         Hashtbl.iter
           (fun k v ->
             let nk = Coord.(k + o) in
             let nv = ((v + d - 1) mod 9) + 1 in
             Hashtbl.add new_table nk nv)
           table);
  { table = new_table; width = width * factor; height = height * factor }

let day15a i = solve (parse i)
let day15b i = parse i |> expand 5 |> solve
