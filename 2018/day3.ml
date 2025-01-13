open Aoclib.Util

let parse i =
  scan_lines i "#%d %@ %d,%d: %dx%d" (fun id x y w h ->
      let hori = Seq.ints x |> Seq.take w in
      let vert = Seq.ints y |> Seq.take h in
      (id, Seq.product hori vert))
  |> List.to_seq

let part1 i =
  let claims = parse i in
  Seq.concat_map snd claims |> SeqExt.tally |> Hashtbl.to_seq_values
  |> SeqExt.count_matches (fun i -> i > 1)

let part2 i =
  let claims = parse i in
  let overlaps = Seq.concat_map snd claims |> SeqExt.tally in
  Seq.find
    (fun (_id, area) -> Seq.for_all (fun p -> Hashtbl.find overlaps p = 1) area)
    claims
  |> Option.get |> fst
