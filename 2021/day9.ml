open Aoclib
open Aoclib.Util

let directions = [ Dir.Up; Dir.Right; Dir.Down; Dir.Left ]

let parse i =
  let { table; _ } =
    read_coord_table i (Fun.compose Option.some digit_of_char)
  in
  table

let get table pos = Hashtbl.find_opt table pos |> Option.value ~default:max_int

let low_points table =
  Hashtbl.to_seq table
  |> Seq.filter (fun (pos, num) ->
         List.for_all (fun d -> num < get table Coord.(pos >> d)) directions)

let day9a i =
  let table = parse i in
  low_points table |> Seq.map snd |> Seq.map (( + ) 1) |> SeqExt.sum

let day9b i =
  let table = parse i in
  let rec basin (pos, num) =
    pos
    :: (List.map (fun d -> Coord.(pos >> d)) directions
       |> List.filter_map (fun pos ->
              let n = get table pos in
              if n > num && n < 9 then Some (pos, n) else None)
       |> List.concat_map basin)
  in
  low_points table
  |> Seq.map (fun x -> basin x |> List.sort_uniq compare |> List.length)
  |> List.of_seq |> List.sort compare |> List.rev |> list_take 3
  |> ListExt.product
