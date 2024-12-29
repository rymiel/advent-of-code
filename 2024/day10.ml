open Aoclib
open Aoclib.Util

let dirs = [ Coord.up; Coord.down; Coord.left; Coord.right ]

let rec nines table from =
  let current = Hashtbl.find table from in
  if current = 9 then [ from ]
  else
    List.map (Coord.( + ) from) dirs
    |> List.filter (fun p -> Hashtbl.find_opt table p = Some (current + 1))
    |> List.concat_map (nines table)

let parse_day10a i =
  (read_coord_table i (fun _ c -> Some (digit_of_char c))).table

let day10a i =
  let table = parse_day10a i in
  Hashtbl.to_seq table
  |> Seq.filter (fun (_, v) -> v = 0)
  |> Seq.map fst
  |> Seq.map (fun p -> nines table p |> List.sort_uniq compare |> List.length)
  |> Seq.fold_left ( + ) 0

let day10b i =
  let table = parse_day10a i in
  Hashtbl.to_seq table
  |> Seq.filter_map (fun (k, v) ->
         if v = 0 then Some (nines table k |> List.length) else None)
  |> Seq.fold_left ( + ) 0
