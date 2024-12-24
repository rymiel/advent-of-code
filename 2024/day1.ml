open Aoclib.Util

let parse i = scan_lines i "%d %d%!" (fun a b -> (a, b)) |> List.split

let day1a (i : in_channel) : int =
  let left, right = parse i in
  let pairs = List.combine (List.sort compare left) (List.sort compare right) in
  List.fold_left (fun n (a, b) -> n + abs (a - b)) 0 pairs

let day1b i =
  let left, right = parse i in
  List.fold_left (fun n i -> n + (i * count_item i right)) 0 left
