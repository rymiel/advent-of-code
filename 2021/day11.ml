open Aoclib
open Aoclib.Util

let directions =
  [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]

let modify table k f =
  match Hashtbl.find_opt table k with
  | Some x -> Hashtbl.replace table k (f x)
  | None -> ()

let increment_all table =
  Hashtbl.filter_map_inplace (fun _ v -> Some (v + 1)) table

let rec flash table =
  let flashed = Hashset.create 1000 in
  Hashtbl.iter (fun pos v -> if v = 10 then Hashset.add flashed pos) table;
  Hashset.iter
    (fun pos ->
      List.iter
        (fun d ->
          modify table Coord.(pos + d) (fun i -> if i <= 9 then i + 1 else i))
        directions;
      Hashtbl.replace table pos 11)
    flashed;
  if not @@ Hashset.is_empty flashed then flash table

let reset_flashed table =
  let counter = ref 0 in
  Hashtbl.filter_map_inplace
    (fun _ v ->
      Some
        (if v > 9 then (
           incr counter;
           0)
         else v))
    table;
  !counter

let step table =
  increment_all table;
  flash table;
  reset_flashed table

let day11a i =
  let { table; _ } =
    read_coord_table i (Fun.compose Option.some digit_of_char)
  in
  Seq.repeat () |> Seq.take 100 |> Seq.map (fun () -> step table) |> SeqExt.sum

let day11b i =
  let { table; _ } =
    read_coord_table i (Fun.compose Option.some digit_of_char)
  in
  Seq.repeat ()
  |> Seq.map (fun () -> step table)
  |> Seq.find_index (fun i -> i = 100)
  |> Option.map (( + ) 1)
  |> Option.get
