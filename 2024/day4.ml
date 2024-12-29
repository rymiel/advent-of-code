open Aoclib
open Aoclib.Util

let parse_day4 i = (read_coord_table i (fun _ -> Option.some)).table

let day4a i =
  let table = parse_day4 i in
  let letters = [ (0, 'X'); (1, 'M'); (2, 'A'); (3, 'S') ] in
  let has_xmas start delta table =
    List.for_all
      (fun (scalar, letter) ->
        Hashtbl.find_opt table Coord.(start + (delta * scalar)) = Some letter)
      letters
  in
  let directions =
    [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]
  in
  let acc = ref 0 in
  Hashtbl.iter
    (fun pos c ->
      if c = 'X' then
        List.iter
          (fun dir -> if has_xmas pos dir table then incr acc)
          directions)
    table;
  !acc

let day4b i =
  let table = parse_day4 i in
  let directions = [ (-1, -1); (1, -1); (-1, 1); (1, 1) ] in
  let has_xmas start table =
    let corners =
      List.map
        (fun delta -> Hashtbl.find_opt table Coord.(start + delta))
        directions
    in
    let ms =
      List.filter
        (fun delta -> Hashtbl.find_opt table Coord.(start + delta) = Some 'M')
        directions
    in
    let result =
      Hashtbl.find_opt table start = Some 'A'
      && count_item (Some 'M') corners = 2
      && count_item (Some 'S') corners = 2
      &&
      let m1x, m1y = List.nth ms 0 in
      let m2x, m2y = List.nth ms 1 in
      m1x = m2x || m1y = m2y
    in
    result
  in
  let acc = ref 0 in
  Hashtbl.iter
    (fun pos c -> if c = 'A' && has_xmas pos table then incr acc)
    table;
  !acc
