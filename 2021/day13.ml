open Aoclib
open Aoclib.Util

let fold_y paper fy =
  Hashset.filter_map
    (fun (x, y) ->
      let ny = if y > fy then (2 * fy) - y else y in
      Some (x, ny))
    paper

let fold_x paper fx =
  Hashset.filter_map
    (fun (x, y) ->
      let nx = if x > fx then (2 * fx) - x else x in
      Some (nx, y))
    paper

let do_fold paper line =
  Scanf.sscanf line "fold along %c=%d" (fun axis n ->
      (match axis with 'x' -> fold_x | 'y' -> fold_y | _ -> failwith "foo")
        paper n)

let print_paper paper =
  let min_x, max_x = Hashset.to_seq paper |> SeqExt.map_minmax fst in
  let min_y, max_y = Hashset.to_seq paper |> SeqExt.map_minmax snd in
  for y = min_y to max_y do
    for x = min_x to max_x do
      print_string (if Hashset.mem paper (x, y) then "\u{2588}" else " ")
    done;
    print_newline ()
  done

let parse i =
  let paper = Hashset.create 1000 in
  let points, folds = In_channel.input_lines i |> partition_at "" in
  List.iter
    (fun p ->
      String.split_on_char ',' p |> List.map int_of_string |> as_pair
      |> Hashset.add paper)
    points;
  (paper, folds)

let day13a i =
  let paper, folds = parse i in
  let folded = do_fold paper (List.hd folds) in
  Hashset.length folded

let day13b i =
  let paper, folds = parse i in
  let folded = List.fold_left do_fold paper folds in
  print_paper folded;
  0
