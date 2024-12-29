open Aoclib
open Aoclib.Util

let parse_day20 i =
  let { Maze.table; width; height; start; goal } =
    read_maze i (fun { start; goal } pos c ->
        if c = 'S' then start := pos;
        if c = 'E' then goal := pos;
        if c = '#' then Some () else None)
  in
  cardinal_pathfind ~width ~height table start goal |> Option.get

let day20a i =
  let path = parse_day20 i in
  let cheat from rest =
    Coord.[ up * 2; right * 2; down * 2; left * 2 ]
    |> List.map (Coord.( + ) from)
    |> List.filter_map (fun s -> List.find_index (( = ) s) rest)
    |> List.filter (( < ) 100)
    |> List.length
  in
  let rec cheats = function [] -> 0 | x :: xs -> cheat x xs + cheats xs in
  cheats path

let day20b i =
  let path = parse_day20 i in
  let cheat from rest =
    rest
    |> List.mapi (fun i x -> (Coord.manhattan from x, i + 1))
    |> List.filter (fun (m, _) -> m <= 20)
    |> List.filter (fun (m, d) -> m <> d)
    |> List.map (fun (m, d) -> d - m)
    |> List.filter (( <= ) 100)
    |> List.length
  in
  let rec cheats = function [] -> 0 | x :: xs -> cheat x xs + cheats xs in
  cheats path
