open Aoclib
open Aoclib.Util

let directions =
  [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]

let adjacent_occupied table pos =
  ListExt.count_matches
    (fun d ->
      Hashtbl.find_opt table Coord.(pos + d) |> Option.value ~default:false)
    directions

let step table =
  let new_table = Hashtbl.copy table in
  Hashtbl.filter_map_inplace
    (fun pos on ->
      Some
        (let on_adj = adjacent_occupied table pos in
         if on_adj = 0 then true else if on_adj >= 4 then false else on))
    new_table;
  new_table

let occupied table = Hashtbl.fold (fun _ v i -> if v then i + 1 else i) table 0
let parse i = read_coord_table i (fun c -> if c = 'L' then Some false else None)

let find_equilibrium step_fn table =
  Seq.iterate step_fn table |> Seq.map occupied |> sliding_pair
  |> Seq.find (fun (a, b) -> a = b)
  |> Option.get |> fst

let day11a i =
  let table = parse i in
  find_equilibrium step table.table

let visibility_lookup { width; height; table } =
  let oob (x, y) = x < 0 || y < 0 || x >= width || y >= height in
  let visible = Hashtbl.create 1_000_000 in
  let rec raycast cur dir =
    let next = Coord.(cur + dir) in
    if oob next then None
    else if Hashtbl.mem table next then Some next
    else raycast next dir
  in
  Hashtbl.iter
    (fun pos _ ->
      directions
      |> List.filter_map (raycast pos)
      |> List.iter (fun n -> Hashtbl.add visible pos n))
    table;
  visible

let visible_occupied table lookup pos =
  Hashtbl.find_all lookup pos
  |> ListExt.count_matches (fun n -> Hashtbl.find table n)

let step2 lookup table =
  let new_table = Hashtbl.copy table in
  Hashtbl.filter_map_inplace
    (fun pos on ->
      Some
        (let on_adj = visible_occupied table lookup pos in
         if on_adj = 0 then true else if on_adj >= 5 then false else on))
    new_table;
  new_table

let day11b i =
  let table = parse i in
  let lookup = visibility_lookup table in
  find_equilibrium (step2 lookup) table.table
