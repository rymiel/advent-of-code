open Aoclib
open Aoclib.Util

let day12a i =
  let lines = In_channel.input_lines i in
  let height = List.length lines in
  let width = List.hd lines |> String.length in
  let table = Hashtbl.create (height * width) in
  let add_row y s =
    List.iteri (fun x c -> Hashtbl.add table (x, y) c) (Util.chars s)
  in
  List.iteri (fun y line -> add_row y line) lines;
  let remaining = Hashtbl.copy table in
  let dirs = [ (-1, 0); (0, -1); (1, 0); (0, 1) ] in
  let flood_fill (ix, iy) chr =
    let region = Hashtbl.create 100 in
    let rec step p =
      if Hashtbl.mem region p then ()
      else
        match Hashtbl.find_opt table p with
        | Some c when c = chr ->
            Hashtbl.add region p ();
            List.iter (fun d -> step Coord.(p + d)) dirs
        | _ -> ()
    in
    step (ix, iy);
    region
  in
  let area region = Hashtbl.length region in
  let perimeter region =
    Hashtbl.fold
      (fun p () i ->
        count_matches (fun d -> not @@ Hashtbl.mem region Coord.(p + d)) dirs
        + i)
      region 0
  in
  let regions = Hashtbl.create 100 in
  while Hashtbl.length remaining > 0 do
    let ix, iy =
      Hashtbl.to_seq_keys remaining |> Seq.uncons |> Option.get |> fst
    in
    let c = Hashtbl.find table (ix, iy) in
    let region = flood_fill (ix, iy) c in
    Hashtbl.replace regions region ();
    Hashtbl.iter (fun pos () -> Hashtbl.remove remaining pos) region
  done;
  Hashtbl.fold
    (fun region () i -> i + (area region * perimeter region))
    regions 0

let day12b i =
  let lines = In_channel.input_lines i in
  let height = List.length lines in
  let width = List.hd lines |> String.length in
  let table = Hashtbl.create (height * width) in
  let add_row y s =
    List.iteri (fun x c -> Hashtbl.add table (x, y) c) (Util.chars s)
  in
  List.iteri (fun y line -> add_row y line) lines;
  let remaining = Hashtbl.copy table in
  let dirs = [ (-1, 0); (0, -1); (1, 0); (0, 1) ] in
  let flood_fill (ix, iy) chr =
    let region = Hashtbl.create 100 in
    let rec step (x, y) =
      if Hashtbl.mem region (x, y) then ()
      else
        match Hashtbl.find_opt table (x, y) with
        | Some c when c = chr ->
            Hashtbl.add region (x, y) ();
            List.iter (fun (dx, dy) -> step (x + dx, y + dy)) dirs
        | _ -> ()
    in
    step (ix, iy);
    region
  in
  let area region = Hashtbl.length region in
  let perimeter_points region =
    Hashtbl.fold
      (fun (x, y) () i ->
        List.filter_map
          (fun (dx, dy) ->
            if Hashtbl.mem region (x + dx, y + dy) then None
            else Some ((x, y), (dx, dy)))
          dirs
        @ i)
      region []
  in
  let choose table =
    Hashtbl.to_seq_keys table |> Seq.uncons |> Option.get |> fst
  in
  let sides region =
    let pp = Hashtbl.create 100 in
    let counter = ref 0 in
    Hashtbl.replace_seq pp
      (Seq.zip (perimeter_points region |> List.to_seq) (Seq.repeat ()));
    while Hashtbl.length pp > 0 do
      let pos, (dx, dy) = choose pp in
      let rec sweep (ix, iy) (dx, dy) side =
        let d = (ix + dx, iy + dy) in
        if Hashtbl.mem pp (d, side) then (
          Hashtbl.remove pp (d, side);
          sweep d (dx, dy) side)
        else ()
      in
      let sweep_dirs =
        if dx <> 0 then ((0, 1), (0, -1)) else ((1, 0), (-1, 0))
      in
      Hashtbl.remove pp (pos, (dx, dy));
      sweep pos (fst sweep_dirs) (dx, dy);
      sweep pos (snd sweep_dirs) (dx, dy);
      incr counter;
      ()
    done;
    !counter
  in
  let regions = Hashtbl.create 100 in
  while Hashtbl.length remaining > 0 do
    let ix, iy = choose remaining in

    let c = Hashtbl.find table (ix, iy) in
    let region = flood_fill (ix, iy) c in
    Hashtbl.replace regions region ();
    Hashtbl.iter (fun pos () -> Hashtbl.remove remaining pos) region
  done;
  Hashtbl.fold (fun region () i -> i + (area region * sides region)) regions 0
