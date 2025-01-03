open Aoclib
open Aoclib.Util

let points matrix =
  Array.to_seq matrix
  |> Seq.mapi (fun y a -> (y, a))
  |> Seq.flat_map (fun (y, a) ->
         String.to_seq a
         |> Seq.mapi (fun x c -> if c = '#' then Some (x, y) else None)
         |> Seq.filter_map Fun.id)
  |> List.of_seq

let shapes =
  [
    [| "####" |];
    [| " # "; "###"; " # " |];
    [| "  #"; "  #"; "###" |];
    [| "#"; "#"; "#"; "#" |];
    [| "##"; "##" |];
  ]
  |> List.map (fun shape -> (shape, points shape))

let get_shape =
  let dispenser = shapes |> List.to_seq |> Seq.cycle |> Seq.to_dispenser in
  fun () -> dispenser () |> Option.get

let day17a i =
  let xdiffs =
    In_channel.input_all i |> String.trim |> String.to_seq
    |> Seq.map (fun c -> if c = '>' then 1 else -1)
    |> Seq.memoize
  in
  let get_xdiff =
    let dispenser = xdiffs |> Seq.cycle |> Seq.to_dispenser in
    fun () -> dispenser () |> Option.get
  in
  let tiles = Hashset.create 1_000_000 in
  let highest = ref 1 in

  let point_collides (x, y) =
    x < 0 || x > 6 || y > 0 || Hashset.mem tiles (x, y)
  in
  let collides points = List.exists point_collides points in

  for _ = 1 to 2022 do
    let shape, points = get_shape () in
    let shape_x = 2 in
    let shape_y = !highest - Array.length shape - 3 in
    let points = List.map (Coord.( + ) (shape_x, shape_y)) points in

    let moving = ref true in
    let points = ref points in
    while !moving do
      let dx = get_xdiff () in
      let mpoints = List.map (Coord.( + ) (dx, 0)) !points in
      let can_move_jet = not @@ collides mpoints in

      if can_move_jet then points := mpoints;

      let mpoints = List.map (Coord.( + ) (0, 1)) !points in
      let can_drop = not @@ collides mpoints in

      if can_drop then points := mpoints else moving := false
    done;
    List.iter (fun pos -> Hashset.add tiles pos) !points;
    highest := min (List.map snd !points |> ListExt.min) !highest
  done;

  ~- (!highest) + 1

let day17b i =
  ignore i;
  failwith "TODO"
