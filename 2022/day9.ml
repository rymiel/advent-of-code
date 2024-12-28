open Aoclib
open Aoclib.Util

let move_tail tail_prev head =
  if Coord.chebyshev head tail_prev <= 1 then tail_prev
  else Coord.(head - tail_prev) |> Pair.map sign |> Coord.( + ) tail_prev

let dir_of_char = function
  | 'U' -> Dir.Up
  | 'R' -> Dir.Right
  | 'D' -> Dir.Down
  | 'L' -> Dir.Left
  | _ -> invalid_arg "dir_of_char"

let day9a i =
  scan_lines i "%c %d" (fun c i -> (c, i))
  |> List.to_seq
  |> Seq.flat_map (fun (c, i) -> Seq.repeat (dir_of_char c) |> Seq.take i)
  |> Seq.scan Coord.( >> ) (0, 0)
  |> Seq.scan move_tail (0, 0)
  |> Seq.fold_left (fun set i -> CoordSet.add i set) CoordSet.empty
  |> CoordSet.cardinal

let day9b i =
  scan_lines i "%c %d" (fun c i -> (c, i))
  |> List.to_seq
  |> Seq.flat_map (fun (c, i) -> Seq.repeat (dir_of_char c) |> Seq.take i)
  |> Seq.scan Coord.( >> ) (0, 0)
  |> Seq.iterate (Seq.scan move_tail (0, 0))
  |> Seq.drop 9 |> head
  |> Seq.fold_left (fun set i -> CoordSet.add i set) CoordSet.empty
  |> CoordSet.cardinal
