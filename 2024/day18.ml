open Aoclib
open Aoclib.Util

let day18a i =
  let input = scan_lines i "%d,%d" (fun a b -> (a, b)) in
  let size, bytes = (71, 1024) in
  (* let size, bytes = 7, 12 in *)
  let start = (0, 0) in
  let goal = (size - 1, size - 1) in
  let table = Hashset.create (size * size) in
  List.to_seq input |> Seq.take bytes |> Seq.iter (Hashset.add table);
  let path = cardinal_pathfind ~width:size ~height:size table start goal in
  match path with
  | None -> failwith "No path found"
  | Some path -> List.length path - 1

let day18b i =
  let input = scan_lines i "%d,%d" (fun a b -> (a, b)) in
  let size = 71 in
  (* let size = 7 in *)
  let start = (0, 0) in
  let goal = (size - 1, size - 1) in
  let fall n =
    let table = Hashset.create (size * size) in
    List.to_seq input |> Seq.take n |> Seq.iter (Hashset.add table);
    cardinal_pathfind ~width:size ~height:size table start goal
  in
  let out (x, y) = Printf.sprintf "%d,%d" x y in
  binary_searchi 1024 (List.length input) (fun n ->
      fall (n + 1) |> Option.is_none)
  |> List.nth input |> out |> print_endline;
  0
