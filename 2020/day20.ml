open Aoclib
open Aoclib.Util

type side = N | E | S | W
type sided = { normal : int; flipped : int }
type tile = { sides : sided * sided * sided * sided; data : string array }

let repr s =
  String.fold_left (fun i c -> (i lsl 1) + if c == '#' then 1 else 0) 0 s

let flip_repr s =
  String.fold_right (fun c i -> (i lsl 1) + if c == '#' then 1 else 0) s 0

let make_sided s : sided = { normal = repr s; flipped = flip_repr s }
let sided_eq a b = a.normal = b.normal
let sided_eq_flipped a b = a.flipped = b.normal
let sided_can_eq a b = sided_eq a b || sided_eq_flipped a b
let sided_flip { normal; flipped } = { normal = flipped; flipped = normal }
let sides = [| N; E; S; W |]

let get_side { sides = n, e, s, w; _ } = function
  | N -> n
  | E -> e
  | S -> s
  | W -> w

let sides_product = Seq.product (Array.to_seq sides) (Array.to_seq sides)
let opposite_side = function N -> S | E -> W | S -> N | W -> E

let coord_of_side =
  Coord.(function N -> north | E -> east | S -> south | W -> south)

let matrix_rotate_cw (in_matrix : string array) =
  let height = Array.length in_matrix in
  let width = String.length in_matrix.(0) in
  Array.init height (fun y ->
      String.init width (fun x -> in_matrix.(width - x - 1).[y]))

let matrix_vflip (in_matrix : string array) =
  in_matrix |> Array.to_list |> List.rev |> Array.of_list

let tile_rotate_cw { sides = n, e, s, w; data } =
  { sides = (sided_flip w, n, sided_flip e, s); data = matrix_rotate_cw data }

let tile_vflip { sides = n, e, s, w; data } =
  { sides = (s, sided_flip e, n, sided_flip w); data = matrix_vflip data }

type fside = bool * side

let fside_rotate_cw (f, s) =
  match s with N -> (f, E) | E -> (not f, S) | S -> (f, W) | W -> (not f, N)

let fside_vflip (f, s) =
  match s with N -> (f, S) | E -> (not f, E) | S -> (f, N) | W -> (not f, W)

type transformation =
  | Identity
  | Rotate90
  | Rotate180
  | Rotate270
  | VFlip
  | HFlip
  | VFlip90
  | VFlip270

let transformations =
  [ Identity; Rotate90; Rotate180; Rotate270; VFlip; HFlip; VFlip90; VFlip270 ]

let decompose_transform ~rotate_cw ~vflip n = function
  | Identity -> n
  | Rotate90 -> rotate_cw n
  | Rotate180 -> rotate_cw (rotate_cw n)
  | Rotate270 -> rotate_cw (rotate_cw (rotate_cw n))
  | VFlip -> vflip n
  | HFlip -> vflip (rotate_cw (rotate_cw n))
  | VFlip90 -> vflip (rotate_cw n)
  | VFlip270 -> vflip (rotate_cw (rotate_cw (rotate_cw n)))

let tile_transform =
  decompose_transform ~rotate_cw:tile_rotate_cw ~vflip:tile_vflip

let fside_transform =
  decompose_transform ~rotate_cw:fside_rotate_cw ~vflip:fside_vflip

let matrix_transform =
  decompose_transform ~rotate_cw:matrix_rotate_cw ~vflip:matrix_vflip

let read_tile lines : tile =
  let data = Array.of_list lines in
  let column n =
    List.map (fun s -> s.[n]) lines |> List.to_seq |> String.of_seq
  in
  let n = data.(0) in
  let s = data.(9) in
  let w = column 0 in
  let e = column 9 in
  { sides = (make_sided n, make_sided e, make_sided s, make_sided w); data }

let parse i =
  let tiles = Hashtbl.create 1000 in
  In_channel.input_lines i |> partition_all ""
  |> List.iter (fun l ->
         let id = Scanf.sscanf (List.hd l) "Tile %d:" Fun.id in
         let tile = read_tile (List.tl l) in
         Hashtbl.replace tiles id tile);
  let connections =
    Hashtbl.to_seq tiles |> permute_pairs
    |> Seq.filter (fun ((_, tile1), (_, tile2)) ->
           Seq.exists
             (fun (side1, side2) ->
               sided_can_eq (get_side tile1 side1) (get_side tile2 side2))
             sides_product)
    |> Seq.map (Pair.map fst)
    |> List.of_seq
  in
  (tiles, connections)

let day20a i =
  let tiles, connections = parse i in
  Hashtbl.to_seq_keys tiles
  |> Seq.filter (fun i -> ListExt.count_matches (Pair.mem i) connections = 2)
  |> SeqExt.product

let day20b i =
  let tiles, connections = parse i in
  let corners =
    Hashtbl.to_seq_keys tiles
    |> Seq.filter (fun i -> ListExt.count_matches (Pair.mem i) connections = 2)
    |> List.of_seq
  in
  let get_tile = Hashtbl.find tiles in
  let transform_tile_in_place id transform =
    Hashtbl.replace tiles id (tile_transform (Hashtbl.find tiles id) transform)
  in

  let neighbours id =
    List.filter (Pair.mem id) connections
    |> List.map (fun (a, b) -> if a = id then b else a)
  in
  let connection id1 id2 =
    let tile1 = get_tile id1 in
    let tile2 = get_tile id2 in
    sides_product
    |> Seq.find_map (fun (s1, s2) ->
           let sd1 = get_side tile1 s1 in
           let sd2 = get_side tile2 s2 in
           if sided_eq sd1 sd2 then Some (s1, s2, false)
           else if sided_eq_flipped sd1 sd2 then Some (s1, s2, true)
           else None)
    |> Option.get
  in
  let required_transformation (current_side, target_side, flipped) =
    let s1 = (false, current_side) in
    let s2 = (flipped, opposite_side target_side) in
    transformations |> List.find (fun t -> s1 = fside_transform s2 t)
  in

  let layout = Hashtbl.create 100 in
  let in_place = Hashtbl.create 100 in

  let configure_to_top_left top_left =
    List.iteri
      (fun i neighbour ->
        let tl_side, _, _ = connection top_left neighbour in
        let required_side = if i = 0 then N else W in
        let trans = required_transformation (required_side, tl_side, false) in
        transform_tile_in_place top_left trans)
      (neighbours top_left);

    Hashtbl.add layout (0, 0) top_left;
    Hashtbl.add in_place top_left (0, 0)
  in

  let top_left = List.nth corners 1 in
  configure_to_top_left top_left;

  let frontier = Queue.create () in
  Queue.push top_left frontier;

  let set_neighbours_in_place current =
    let set_in_place neighbour =
      let side1, side2, flipped = connection current neighbour in
      let trans = required_transformation (side1, side2, flipped) in
      transform_tile_in_place neighbour trans;
      let place = Coord.(Hashtbl.find in_place current + coord_of_side side1) in
      Hashtbl.add in_place neighbour place;
      Hashtbl.add layout place neighbour;
      Queue.push neighbour frontier
    in
    neighbours current
    |> List.filter (Fun.negate (Hashtbl.mem in_place))
    |> List.iter set_in_place
  in

  while not @@ Queue.is_empty frontier do
    let front = Queue.pop frontier in
    set_neighbours_in_place front
  done;

  let square_side =
    Hashtbl.length tiles |> float_of_int |> sqrt |> int_of_float
  in
  let tile_side = 10 in
  let new_tile_side = 8 in
  let new_square_side = new_tile_side * square_side in
  let new_square_set = Hashset.create 1_000_000 in
  for square_y = 0 to square_side - 1 do
    let row_tiles =
      Seq.ints 0 |> Seq.take square_side
      |> Seq.map (fun x -> Hashtbl.find layout (x, square_y))
      |> Array.of_seq
    in
    for tile_y = 1 to tile_side - 2 do
      for square_x = 0 to square_side - 1 do
        for tile_x = 1 to tile_side - 2 do
          if (get_tile row_tiles.(square_x)).data.(tile_y).[tile_x] = '#' then
            let new_x = (square_x * new_tile_side) + (tile_x - 1) in
            let new_y = (square_y * new_tile_side) + (tile_y - 1) in
            Hashset.add new_square_set (new_x, new_y)
        done
      done
    done
  done;
  let new_square_matrix =
    Array.init new_square_side (fun y ->
        String.init new_square_side (fun x ->
            if Hashset.mem new_square_set (x, y) then '#' else '.'))
  in

  let sea_monster =
    [ "                  # "; "#    ##    ##    ###"; " #  #  #  #  #  #   " ]
  in
  let sea_monster_parts =
    ListExt.map_sum (fun s -> count_char '#' s) sea_monster
  in

  let in_matrix matrix x y =
    if y >= Array.length matrix then false
    else
      let s = matrix.(y) in
      if x >= String.length s then false else s.[x] = '#'
  in

  let sea_monster_at matrix ix iy =
    let monster_coords = Hashset.create 100 in
    List.iteri
      (fun dy s ->
        String.iteri
          (fun dx c ->
            let x = dx + ix in
            let y = dy + iy in
            if c = '#' && in_matrix matrix x y then
              Hashset.add monster_coords (x, y))
          s)
      sea_monster;
    if Hashset.length monster_coords = sea_monster_parts then
      Some monster_coords
    else None
  in

  let side_seq = Seq.ints 0 |> Seq.take new_square_side in
  let matrix_squares =
    ArrayExt.map_sum (fun s -> count_char '#' s) new_square_matrix
  in

  transformations
  |> List.map (fun trans ->
         let matrix = matrix_transform new_square_matrix trans in
         Seq.product side_seq side_seq
         |> Seq.filter_map (fun (x, y) -> sea_monster_at matrix x y)
         |> Seq.fold_left
              (fun a b ->
                Hashset.add_seq a (Hashset.to_seq b);
                a)
              (Hashset.create 0)
         |> Hashset.length)
  |> List.find (( <> ) 0)
  |> ( - ) matrix_squares
