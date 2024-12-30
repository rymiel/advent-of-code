open Aoclib
open Aoclib.Util

let ray f d = Seq.iterate (Coord.( + ) d) f

let line f t =
  let dir = Coord.(t - f) |> Pair.map sign in
  let head = ray f dir |> Seq.take_while (( <> ) t) in
  let tail = Seq.return t in
  Seq.append head tail

let checks = [ (0, 1); (-1, 1); (1, 1) ]

let rec sand_drop_void floor table pos =
  if snd pos > floor then None
  else
    List.map (Coord.( + ) pos) checks
    |> List.find_opt (Fun.negate (Hashset.mem table))
    |> Option.fold ~none:(Some pos) ~some:(sand_drop_void floor table)

let parse i =
  let input =
    In_channel.input_lines i
    |> List.map (fun s -> scan_all_str s "%d,%d%[^0-9\n]" (fun a b _ -> (a, b)))
  in
  let table = Hashset.create 1_000_000 in
  List.iter
    (fun l ->
      List.to_seq l |> sliding_pair
      |> Seq.iter (fun (a, b) -> Seq.iter (Hashset.add table) (line a b)))
    input;
  table

let day14a i =
  let table = parse i in
  let maxy = Hashset.to_seq table |> Seq.map snd |> SeqExt.max in
  let do_drop () =
    let n = sand_drop_void maxy table (500, 0) in
    Option.iter (fun n -> Hashset.add table n) n;
    n
  in
  Seq.forever do_drop |> Seq.take_while Option.is_some |> Seq.length

let rec sand_drop_floor floor table pos =
  if snd pos = floor - 1 then pos
  else
    List.map (Coord.( + ) pos) checks
    |> List.find_opt (Fun.negate (Hashset.mem table))
    |> Option.fold ~none:pos ~some:(sand_drop_floor floor table)

let day14b i =
  let table = parse i in
  let floor = (Hashset.to_seq table |> Seq.map snd |> SeqExt.max) + 2 in
  let do_drop () =
    let n = sand_drop_floor floor table (500, 0) in
    Hashset.add table n;
    n
  in
  (Seq.forever do_drop |> Seq.take_while (( <> ) (500, 0)) |> Seq.length) + 1
