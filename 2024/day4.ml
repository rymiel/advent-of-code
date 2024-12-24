open Aoclib
open Aoclib.Util

let parse_day4 i =
  let lines = In_channel.input_lines i in
  let add_row y m s =
    fold_lefti (fun m c x -> CoordMap.add (x, y) c m) m (chars s)
  in
  fold_lefti (fun m line y -> add_row y m line) CoordMap.empty lines

let day4a i =
  let map = parse_day4 i in
  let letters = [ (0, 'X'); (1, 'M'); (2, 'A'); (3, 'S') ] in
  let has_xmas start delta map =
    List.for_all
      (fun (scalar, letter) ->
        CoordMap.find_opt Coord.(start + (delta * scalar)) map = Some letter)
      letters
  in
  let directions =
    [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]
  in
  let acc = ref 0 in
  CoordMap.iter
    (fun pos c ->
      if c = 'X' then
        List.iter (fun dir -> if has_xmas pos dir map then incr acc) directions)
    map;
  !acc

let day4b i =
  let map = parse_day4 i in
  let directions = [ (-1, -1); (1, -1); (-1, 1); (1, 1) ] in
  let has_xmas start map =
    let corners =
      List.map
        (fun delta -> CoordMap.find_opt Coord.(start + delta) map)
        directions
    in
    let ms =
      List.filter
        (fun delta -> CoordMap.find_opt Coord.(start + delta) map = Some 'M')
        directions
    in
    let result =
      CoordMap.find_opt start map = Some 'A'
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
  CoordMap.iter (fun pos c -> if c = 'A' && has_xmas pos map then incr acc) map;
  !acc
