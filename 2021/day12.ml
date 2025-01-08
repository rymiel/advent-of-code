open Aoclib.Util

let is_upper = function 'A' .. 'Z' -> true | _ -> false
let is_repeatable s = is_upper s.[0]

let parse i =
  let graph = Hashtbl.create 1000 in
  In_channel.input_lines i
  |> List.map (fun line -> String.split_on_char '-' line |> as_pair)
  |> List.iter (fun (a, b) ->
         if b <> "start" then Hashtbl.add graph a b;
         if a <> "start" then Hashtbl.add graph b a);
  graph

let day12a i =
  let graph = parse i in
  let rec dfs current visited =
    if current = "end" then 1
    else
      Hashtbl.find_all graph current
      |> List.filter (fun n -> not @@ StringSet.mem n visited)
      |> ListExt.map_sum (fun n ->
             let next_visited =
               if is_repeatable n then visited else StringSet.add n visited
             in
             dfs n next_visited)
  in
  dfs "start" StringSet.empty

module StringMap = Map.Make (String)

let day12b i =
  let graph = parse i in
  let get map key = StringMap.find_opt key map |> Option.value ~default:0 in
  let rec dfs current visited =
    if current = "end" then 1
    else
      let double_used = StringMap.exists (fun _ v -> v >= 2) visited in
      Hashtbl.find_all graph current
      |> List.filter (fun n -> get visited n < if double_used then 1 else 2)
      |> ListExt.map_sum (fun n ->
             let next_visited =
               if is_repeatable n then visited
               else StringMap.add n (get visited n + 1) visited
             in
             dfs n next_visited)
  in
  dfs "start" StringMap.empty
