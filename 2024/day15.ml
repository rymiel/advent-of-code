open Aoclib
open Aoclib.Util

type thing = Box | Wall | Air

let direction = function
  | '^' -> Dir.Up
  | '>' -> Dir.Right
  | 'v' -> Dir.Down
  | '<' -> Dir.Left
  | _ -> failwith "invalid dir"

let day15a i =
  let map_lines, path_lines = partition_at "" (In_channel.input_lines i) in
  let path = String.concat "" path_lines |> chars |> List.map direction in
  let table = Hashtbl.create 1000 in
  let robot = ref (0, 0) in
  let add_row y s =
    List.iteri
      (fun x c ->
        Hashtbl.replace table (x, y)
          (match c with
          | '#' -> Wall
          | 'O' -> Box
          | '.' -> Air
          | '@' ->
              robot := (x, y);
              Air
          | _ -> failwith "invalid thing"))
      (chars s)
  in
  List.iteri add_row map_lines;
  let move from target =
    Hashtbl.replace table from Air;
    Hashtbl.replace table target Box;
    true
  in
  let rec push from dir =
    let target = Coord.(from >> dir) in
    match Hashtbl.find table target with
    | Air -> move from target
    | Wall -> false
    | Box -> if push target dir then move from target else false
  in
  let simulate move =
    let target = Coord.(!robot >> move) in
    match Hashtbl.find table target with
    | Air -> robot := target
    | Wall -> ()
    | Box -> if push target move then robot := target else ()
  in
  List.iter simulate path;
  Hashtbl.fold
    (fun (x, y) v i -> i + if v = Box then x + (100 * y) else 0)
    table 0

type wide_thing = BoxLeft | BoxRight | Wall | Air

let day15b i =
  let map_lines, path_lines = partition_at "" (In_channel.input_lines i) in
  let path = String.concat "" path_lines |> chars |> List.map direction in
  let table = Hashtbl.create 1000 in
  let robot = ref (0, 0) in
  let add_row y s =
    List.iteri
      (fun x c ->
        let left, right =
          match c with
          | '#' -> (Wall, Wall)
          | 'O' -> (BoxLeft, BoxRight)
          | '.' -> (Air, Air)
          | '@' ->
              robot := (x * 2, y);
              (Air, Air)
          | _ -> failwith "invalid thing"
        in
        Hashtbl.replace table (x * 2, y) left;
        Hashtbl.replace table ((x * 2) + 1, y) right)
      (chars s)
  in
  List.iteri add_row map_lines;
  let box_sides from =
    let c = Hashtbl.find table from in
    match c with
    | BoxLeft -> (from, Coord.(from + right))
    | BoxRight -> (Coord.(from + left), from)
    | _ -> failwith "invalid box_sides"
  in
  let move from dir =
    let boxl, boxr = box_sides from in
    Hashtbl.replace table boxl Air;
    Hashtbl.replace table boxr Air;
    Hashtbl.replace table Coord.(boxl >> dir) BoxLeft;
    Hashtbl.replace table Coord.(boxr >> dir) BoxRight
  in
  let rec can_push from dir =
    match dir with
    | Dir.Left | Dir.Right -> check_push Coord.(from >> dir) dir
    | Dir.Down | Dir.Up ->
        let boxl, boxr = box_sides from in
        check_push boxl dir && check_push boxr dir
  and check_push from dir =
    let target = Coord.(from >> dir) in
    match Hashtbl.find table target with
    | Air -> true
    | Wall -> false
    | BoxLeft | BoxRight -> can_push target dir
  in
  let rec do_push from dir =
    match dir with
    | Dir.Left | Dir.Right ->
        chain_push Coord.(from >> dir) dir;
        move from dir
    | Dir.Down | Dir.Up ->
        let boxl, boxr = box_sides from in
        chain_push boxl dir;
        chain_push boxr dir;
        move from dir
  and chain_push from dir =
    let target = Coord.(from >> dir) in
    match Hashtbl.find table target with
    | Air | Wall -> ()
    | BoxLeft | BoxRight -> do_push target dir
  in
  let simulate move =
    let target = Coord.(!robot >> move) in
    match Hashtbl.find table target with
    | Air -> robot := target
    | Wall -> ()
    | BoxLeft | BoxRight ->
        if can_push target move then (
          do_push target move;
          robot := target)
        else ()
  in
  List.iter simulate path;
  Hashtbl.fold
    (fun (x, y) v i -> i + if v = BoxLeft then x + (100 * y) else 0)
    table 0
