open Aoclib.Util

let () =
  let problem = Sys.argv.(1) in
  let variant = Sys.argv.(2) in
  let solver : solver =
    match (problem, variant) with
    | "day1", "a" -> Day1.day1a
    | "day1", "b" -> Day1.day1b
    | "day2", "a" -> Day2.day2a
    | "day2", "b" -> Day2.day2b
    | "day3", "a" -> Day3.day3a
    | "day3", "b" -> Day3.day3b
    | "day4", "a" -> Day4.day4a
    | "day4", "b" -> Day4.day4b
    | "day13", "a" -> Day13.day13a
    | "day13", "b" -> failwith "todo"
    | "day15", "a" -> Day15.day15a
    | "day15", "b" -> Day15.day15b
    | _ -> failwith (Printf.sprintf "Unknown problem %s %s" problem variant)
  in
  solve 2020 solver problem
