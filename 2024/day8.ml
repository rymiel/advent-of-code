open Aoclib.Util
open Aoclib

let day8a i =
  let map = CharMap2D.read_matrix i '.' in
  let antinodes = Hashtbl.create 100 in
  CharMap2D.iter
    (fun c p ->
      CharMap2D.iter
        (fun c' p' ->
          if c = c' && p <> p' then
            let diff = Coord.(p - p') in
            let antinode = Coord.(p + diff) in
            if CharMap2D.in_bounds antinode map then
              Hashtbl.replace antinodes antinode ())
        map)
    map;
  Hashtbl.length antinodes

let day8b i =
  let map = CharMap2D.read_matrix i '.' in
  let antinodes = Hashtbl.create 100 in
  CharMap2D.iter
    (fun c p ->
      CharMap2D.iter
        (fun c' p' ->
          if c = c' && p <> p' then
            let step = Coord.(p - p') in
            let antinode = ref p in
            while CharMap2D.in_bounds !antinode map do
              Hashtbl.replace antinodes !antinode ();
              antinode := Coord.(!antinode + step)
            done)
        map)
    map;
  Hashtbl.length antinodes
