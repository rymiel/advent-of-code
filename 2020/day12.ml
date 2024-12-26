open Aoclib
open Aoclib.Util

let rotate_degrees dir turn degrees =
  let n = degrees / 90 in
  Seq.ints 0 |> Seq.take n |> Seq.fold_left (fun d _ -> turn d) dir

let step (pos, dir) (action, value) =
  match action with
  | 'N' -> (Coord.(pos + (north * value)), dir)
  | 'S' -> (Coord.(pos + (south * value)), dir)
  | 'E' -> (Coord.(pos + (east * value)), dir)
  | 'W' -> (Coord.(pos + (west * value)), dir)
  | 'L' -> (pos, rotate_degrees dir Dir.rotate_ccw value)
  | 'R' -> (pos, rotate_degrees dir Dir.rotate_cw value)
  | 'F' -> (Coord.(pos + Dir.scale dir value), dir)
  | _ -> invalid_arg "action"

let solve i step initial =
  scan_lines i "%c%d" (fun c i -> (c, i))
  |> List.fold_left step initial
  |> fst
  |> Coord.manhattan (0, 0)

let rotate_coord_cw (x, y) = (-y, x)
let rotate_coord_ccw (x, y) = (y, -x)

let step_waypoint (ship, waypoint) (action, value) =
  match action with
  | 'N' -> (ship, Coord.(waypoint + (north * value)))
  | 'S' -> (ship, Coord.(waypoint + (south * value)))
  | 'E' -> (ship, Coord.(waypoint + (east * value)))
  | 'W' -> (ship, Coord.(waypoint + (west * value)))
  | 'L' -> (ship, rotate_degrees waypoint rotate_coord_ccw value)
  | 'R' -> (ship, rotate_degrees waypoint rotate_coord_cw value)
  | 'F' -> (Coord.(ship + (waypoint * value)), waypoint)
  | _ -> invalid_arg "action"

let day12a i = solve i step ((0, 0), Dir.Right)
let day12b i = solve i step_waypoint ((0, 0), (10, -1))
