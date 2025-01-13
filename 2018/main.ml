open Aoclib.Util

module type Day = sig
  val part1 : solver
  val part2 : solver
end

let days : (module Day) list = [ (module Day1); (module Day2); (module Day3) ]

let () =
  let problem = Sys.argv.(1) in
  let variant = Sys.argv.(2) in
  let day_idx = Scanf.sscanf problem "day%d" (fun i -> i - 1) in
  let module Selected = (val List.nth days day_idx) in
  let solver =
    match variant with
    | "a" -> Selected.part1
    | "b" -> Selected.part2
    | _ -> failwith (Printf.sprintf "Unknown problem %s %s" problem variant)
  in
  solve 2018 solver problem
