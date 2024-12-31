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
    | "day5", "a" -> Day5.day5a
    | "day5", "b" -> Day5.day5b
    | "day6", "a" -> Day6.day6a
    | "day6", "b" -> Day6.day6b
    | "day7", "a" -> Day7.day7a
    | "day7", "b" -> Day7.day7b
    | "day8", "a" -> Day8.day8a
    | "day8", "b" -> Day8.day8b
    | "day9", "a" -> Day9.day9a
    | "day9", "b" -> Day9.day9b
    | "day10", "a" -> Day10.day10a
    | "day10", "b" -> Day10.day10b
    | "day11", "a" -> Day11.day11a
    | "day11", "b" -> Day11.day11b
    | "day12", "a" -> Day12.day12a
    | "day12", "b" -> Day12.day12b
    | "day13", "a" -> Day13.day13a
    | "day13", "b" -> Day13.day13b
    | "day14", "a" -> Day14.day14a
    | "day14", "b" -> Day14.day14b
    | "day15", "a" -> Day15.day15a
    | "day15", "b" -> Day15.day15b
    | "day16", "a" -> Day16.day16a
    | "day16", "b" -> Day16.day16b
    | "day17", "a" -> Day17.day17a
    | "day17", "b" -> Day17.day17b
    | "day18", "a" -> Day18.day18a
    | "day18", "b" -> Day18.day18b
    | "day19", "a" -> Day19.day19a
    | "day19", "b" -> Day19.day19b
    | "day20", "a" -> Day20.day20a
    | "day20", "b" -> Day20.day20b
    | _ -> failwith (Printf.sprintf "Unknown problem %s %s" problem variant)
  in
  solve 2020 solver problem
