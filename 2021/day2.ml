open Aoclib
open Aoclib.Util

let parse i =
  scan_lines i "%s %d" (fun command i ->
      match command with
      | "forward" -> (i, 0)
      | "down" -> (0, i)
      | "up" -> (0, -i)
      | _ -> failwith "invalid")

let day2a i = parse i |> List.fold_left Coord.( + ) (0, 0) |> Pair.apply ( * )

let day2b i =
  parse i
  |> List.fold_left
       (fun (aim, pos) (f, v) ->
         let new_aim = aim + v in
         let deepen = new_aim * f in
         (new_aim, Coord.(pos + (f, deepen))))
       (0, (0, 0))
  |> snd |> Pair.apply ( * )
