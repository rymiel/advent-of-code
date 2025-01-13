open Aoclib.Util

let () =
  let problem = Sys.argv.(1) in
  let variant = Sys.argv.(2) in
  let solver : solver =
    match (problem, variant) with
    | "day1", "a" -> Day1.day1a
    | "day1", "b" -> Day1.day1b
    | _ -> failwith (Printf.sprintf "Unknown problem %s %s" problem variant)
  in
  solve 2018 solver problem
