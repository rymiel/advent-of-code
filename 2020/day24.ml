open Aoclib
open Aoclib.Util

let e, se, sw, w, nw, ne = ((1, 0), (0, 1), (-1, 1), (-1, 0), (0, -1), (1, -1))
let directions = [ e; se; sw; w; nw; ne ]

let rec read_directions n str =
  if String.length str = n then []
  else
    match str.[n] with
    | 'e' -> e :: read_directions (n + 1) str
    | 'w' -> w :: read_directions (n + 1) str
    | c -> (
        match (c, str.[n + 1]) with
        | 's', 'e' -> se :: read_directions (n + 2) str
        | 's', 'w' -> sw :: read_directions (n + 2) str
        | 'n', 'e' -> ne :: read_directions (n + 2) str
        | 'n', 'w' -> nw :: read_directions (n + 2) str
        | _, _ -> failwith "invalid")

let adjacent_active table pos =
  ListExt.count_matches (fun d -> Hashset.mem table Coord.(pos + d)) directions

let step table =
  let new_table = Hashset.copy table in
  Hashset.filter_inplace
    (fun pos ->
      let on_adj = adjacent_active table pos in
      if on_adj = 0 || on_adj > 2 then false else true)
    new_table;
  let frontier = Hashset.create (Hashset.length table) in
  Hashset.iter
    (fun pos ->
      List.iter
        (fun d ->
          let n = Coord.(pos + d) in
          if not @@ Hashset.mem table n then Hashset.add frontier n)
        directions)
    table;
  Hashset.iter
    (fun pos ->
      let on_adj = adjacent_active table pos in
      if on_adj = 2 then Hashset.add new_table pos)
    frontier;
  new_table

let parse i =
  let set = Hashset.create 1000 in
  let flip tile =
    if Hashset.mem set tile then Hashset.remove set tile
    else Hashset.add set tile
  in
  In_channel.input_lines i
  |> List.map (read_directions 0)
  |> List.map (List.fold_left Coord.( + ) (0, 0))
  |> List.iter flip;
  set

let day24a i =
  let set = parse i in
  Hashset.length set

let day24b i =
  let set = parse i in
  Seq.iterate step set |> Seq.drop 100 |> head |> Hashset.length
