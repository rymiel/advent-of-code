open Aoclib.Util

let parse i = scan_lines i "%d,%d -> %d,%d" (fun a b c d -> ((a, b), (c, d)))

let day5a i =
  let lines = parse i in
  let straight_line_range ((xa, ya), (xb, yb)) =
    if xa = xb then Some (range ya yb |> Seq.map (fun y -> (xa, y)))
    else if ya = yb then Some (range xa xb |> Seq.map (fun x -> (x, ya)))
    else None
  in
  List.to_seq lines
  |> Seq.filter_map straight_line_range
  |> Seq.concat |> SeqExt.tally |> Hashtbl.to_seq_values
  |> Seq.filter (fun i -> i >= 2)
  |> Seq.length

let day5b i =
  let lines = parse i in
  let line_range ((xa, ya), (xb, yb)) =
    if xa = xb then range ya yb |> Seq.map (fun y -> (xa, y))
    else if ya = yb then range xa xb |> Seq.map (fun x -> (x, ya))
    else Seq.zip (range xa xb) (range ya yb)
  in
  List.to_seq lines |> Seq.flat_map line_range |> SeqExt.tally
  |> Hashtbl.to_seq_values
  |> Seq.filter (fun i -> i >= 2)
  |> Seq.length
