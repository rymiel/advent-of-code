open Aoclib
open Aoclib.Util

let sides =
  [ (1, 0, 0); (0, 1, 0); (0, 0, 1); (-1, 0, 0); (0, -1, 0); (0, 0, -1) ]

let parse i =
  let points = Hashset.create 1_000_000 in
  scan_lines i "%d,%d,%d" (fun x y z -> (x, y, z))
  |> List.iter (Hashset.add points);
  points

let day18a i =
  let points = parse i in
  Hashset.to_seq points
  |> SeqExt.map_sum (fun pos ->
         ListExt.count_matches
           (fun d -> not @@ Hashset.mem points Coord3.(pos + d))
           sides)

let day18b i =
  let points = parse i in
  let min_x, max_x = Hashset.to_seq points |> SeqExt.map_minmax Coord3.x in
  let min_y, max_y = Hashset.to_seq points |> SeqExt.map_minmax Coord3.y in
  let min_z, max_z = Hashset.to_seq points |> SeqExt.map_minmax Coord3.z in
  let oob (x, y, z) =
    x < min_x || y < min_y || z < min_z || x > max_x || y > max_y || z > max_z
  in
  let air = Hashtbl.create 1000 in
  let exposed_to_air pos =
    match Hashtbl.find_opt air pos with
    | Some x -> x
    | None ->
        let ctx = Hashset.create 1000 in
        let rec aux pos =
          Hashset.add ctx pos;
          oob pos
          || List.exists
               (fun d ->
                 let n = Coord3.(pos + d) in
                 (not @@ Hashset.mem points n)
                 && (not @@ Hashset.mem ctx n)
                 && aux n)
               sides
        in
        let r = aux pos in
        Hashset.iter (fun p -> Hashtbl.replace air p r) ctx;
        r
  in
  Hashset.to_seq points
  |> SeqExt.map_sum (fun pos ->
         ListExt.count_matches
           (fun d ->
             let n = Coord3.(pos + d) in
             (not @@ Hashset.mem points n) && exposed_to_air n)
           sides)
