open Aoclib.Util

let read_container table line =
  let container, contained =
    Str.split (Str.regexp_string " contain ") line |> as_pair
  in
  if contained <> "no other bags." then
    let container = Scanf.sscanf container "%s %s bags%!" (fun a b -> a ^ b) in
    String.split_on_char ',' contained
    |> List.iter (fun s ->
           Scanf.sscanf s " %d %s %s bag" (fun i a b ->
               Hashtbl.add table container (i, a ^ b)))

let parse i =
  let table = Hashtbl.create 1_000_000 in
  In_channel.input_lines i |> List.iter (read_container table);
  table

let day7a i =
  let table = parse i in
  let rec eventually_contains target current =
    Hashtbl.find_all table current
    |> List.exists (fun (_, i) -> i = target || eventually_contains target i)
  in
  count_matches
    (fun key -> eventually_contains "shinygold" key)
    (Hashtbl.to_seq_keys table |> List.of_seq |> List.sort_uniq compare)

let day7b i =
  let table = parse i in
  let rec contains_total current =
    Hashtbl.find_all table current
    |> List.map (fun (i, b) -> i * (contains_total b + 1))
    |> sum
  in
  contains_total "shinygold"
