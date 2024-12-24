open Aoclib.Util

let day11 target i =
  let nums =
    In_channel.input_all i |> String.trim |> String.split_on_char ' '
    |> List.map int_of_string
  in
  let cache = Hashtbl.create 1_000_000 in
  let rec blink n step =
    let aux () =
      if n = 0 then blink 1 (step + 1)
      else
        let s = string_of_int n in
        let len = String.length s in
        if len mod 2 = 0 then
          let half = len / 2 in
          let left = int_of_string (String.sub s 0 half) in
          let right = int_of_string (String.sub s half half) in
          blink left (step + 1) + blink right (step + 1)
        else blink (n * 2024) (step + 1)
    in
    if step = target then 1
    else
      match Hashtbl.find_opt cache (n, step) with
      | Some x -> x
      | None ->
          let r = aux () in
          Hashtbl.replace cache (n, step) r;
          r
  in
  List.map (fun n -> blink n 0) nums |> sum

let day11a = day11 25
let day11b = day11 75
