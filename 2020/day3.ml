open Aoclib.Util

let day3a i =
  let map = BinaryMap2D.read_matrix i '#' in
  let ys = List.init map.height (fun i -> i) in
  count_matches
    (fun y ->
      let x = y * 3 mod map.width in
      BinaryMap2D.has (x, y) map)
    ys

let day3b i =
  let map = BinaryMap2D.read_matrix i '#' in
  let ys = List.init map.height (fun i -> i) in
  let xs = List.init ((map.height / 2) + 1) (fun i -> i) in
  let has_mod (x, y) =
    BinaryMap2D.has (x mod map.width, y mod map.height) map
  in
  let slopes =
    [
      (fun y -> (y, y));
      (fun y -> (y * 3, y));
      (fun y -> (y * 5, y));
      (fun y -> (y * 7, y));
    ]
  in
  let trees =
    List.map (fun fn -> count_matches (fun y -> has_mod (fn y)) ys) slopes
  in
  let trees_x = count_matches (fun x -> has_mod (x, x * 2)) xs in
  trees_x * product trees
