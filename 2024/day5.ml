open Aoclib.Util

let parse i =
  let rules, input = partition_at "" (In_channel.input_lines i) in
  let rules =
    rules
    |> List.map (fun s ->
           String.split_on_char '|' s |> List.map int_of_string |> as_pair)
  in
  let input =
    input
    |> List.map (fun s ->
           String.split_on_char ',' s |> List.map int_of_string)
  in
  (rules, input)

let check_forward current rest rules =
  not @@ List.exists (fun r -> List.mem (r, current) rules) rest

let rec correct rules = function
  | [] -> true
  | x :: xs -> check_forward x xs rules && correct rules xs

let middle_value l = List.nth l (List.length l / 2)

let day5a i =
  let rules, input = parse i in
  let correct_input = List.filter (correct rules) input in
  List.map middle_value correct_input |> sum

let day5b i =
  let rules, input = parse i in
  let incorrect_input = List.filter (fun l -> not @@ correct rules l) input in
  let rule_compare a b =
    if List.mem (a, b) rules then -1
    else if List.mem (b, a) rules then 1
    else 0
  in
  let fix = List.stable_sort rule_compare in
  let corrected = List.map fix incorrect_input in
  List.map middle_value corrected |> sum