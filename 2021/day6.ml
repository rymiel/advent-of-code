open Aoclib.Util

let rec fish_count =
  let cache = Hashtbl.create 1000 in
  fun num steps ->
    if steps = 0 then 1
    else
      match Hashtbl.find_opt cache (num, steps) with
      | Some r -> r
      | None ->
          let r =
            if num = 0 then fish_count 6 (steps - 1) + fish_count 8 (steps - 1)
            else fish_count (num - 1) (steps - 1)
          in
          Hashtbl.add cache (num, steps) r;
          r

let day6 limit i =
  let initial =
    In_channel.input_all i |> String.trim |> String.split_on_char ','
    |> List.map int_of_string
  in
  ListExt.map_sum (fun i -> fish_count i limit) initial

let day6a = day6 80
let day6b = day6 256
