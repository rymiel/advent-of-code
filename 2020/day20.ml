open Aoclib
open Aoclib.Util

type tile = {
  n : int;
  e : int;
  s : int;
  w : int;
  nf : int;
  ef : int;
  sf : int;
  wf : int;
}

let repr s =
  String.fold_left (fun i c -> (i lsl 1) + if c == '#' then 1 else 0) 0 s

let flip_repr s =
  String.fold_right (fun c i -> (i lsl 1) + if c == '#' then 1 else 0) s 0

let read_tile lines : tile =
  let column n =
    List.map (fun s -> s.[n]) lines |> List.to_seq |> String.of_seq
  in
  let n = List.nth lines 0 in
  let s = List.nth lines 9 in
  let w = column 0 in
  let e = column 9 in
  {
    n = repr n;
    e = repr e;
    s = repr s;
    w = repr w;
    nf = flip_repr n;
    ef = flip_repr e;
    sf = flip_repr s;
    wf = flip_repr w;
  }

let day20a i =
  let tiles =
    In_channel.input_lines i |> partition_all ""
    |> List.map (fun l ->
           let id = Scanf.sscanf (List.hd l) "Tile %d:" Fun.id in
           let tile = read_tile (List.tl l) in
           (id, tile))
  in
  let sides =
    [
      (fun t -> (t.n, t.nf));
      (fun t -> (t.e, t.ef));
      (fun t -> (t.s, t.sf));
      (fun t -> (t.w, t.wf));
    ]
  in
  let connections =
    List.to_seq tiles |> permute_pairs
    |> Seq.concat_map (fun ((tile1_id, tile1_data), (tile2_id, tile2_data)) ->
           Seq.product (List.to_seq sides) (List.to_seq sides)
           |> Seq.filter_map (fun (side_a, side_b) ->
                  let a = side_a tile1_data in
                  let b = side_b tile2_data in
                  if a = b || Pair.flip a = b then Some (tile1_id, tile2_id)
                  else None))
    |> List.of_seq
  in
  List.map fst tiles
  |> List.filter (fun i ->
         ListExt.count_matches (fun (a, b) -> a = i || b = i) connections = 2)
  |> ListExt.product

let day20b i =
  ignore i;
  failwith "TODO"
