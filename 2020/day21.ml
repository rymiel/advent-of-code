open Aoclib
open Aoclib.Util

let parse i =
  let read_line line =
    let ingredients, allergens = String.split_on_char '(' line |> as_pair in
    let ingredients = String.trim ingredients |> String.split_on_char ' ' in
    let allergens =
      String.sub allergens 9 (String.length allergens - 10)
      |> String.split_on_char ',' |> List.map String.trim
    in
    (ingredients, allergens)
  in
  In_channel.input_lines i |> List.map read_line

let day21a i =
  let input = parse i in
  let can_be_allergen ingredient allergen =
    List.filter (fun (_, a) -> List.mem allergen a) input
    |> List.for_all (fun (i, _) -> List.mem ingredient i)
  in
  let could_be_allergen = Hashset.create 1_000_000 in
  List.iter
    (fun (ingredients, allergens) ->
      List.iter
        (fun i ->
          List.iter
            (fun a ->
              if can_be_allergen i a then Hashset.add could_be_allergen i)
            allergens)
        ingredients)
    input;
  List.concat_map fst input
  |> ListExt.count_matches (Fun.negate (Hashset.mem could_be_allergen))

let day21b i =
  let input = parse i in
  let all_allergens = List.concat_map snd input |> List.sort_uniq compare in
  (* literally copied from day 16 *)
  let rec resolve = function
    | [] -> []
    | fields ->
        let up, rest =
          List.partition (fun (_, s) -> StringSet.cardinal s = 1) fields
        in
        let res = List.map (fun (i, s) -> (i, StringSet.min_elt s)) up in
        let skip s =
          List.fold_left (fun s (_, e) -> StringSet.remove e s) s res
        in
        let rest = (List.map (fun (i, s) -> (i, skip s))) rest in
        res @ resolve rest
  in
  let shared_allergens =
    List.map
      (fun allergen ->
        let set =
          List.filter (fun (_, a) -> List.mem allergen a) input
          |> List.map fst |> List.map StringSet.of_list
          |> ListExt.fold_left' StringSet.inter
        in
        (allergen, set))
      all_allergens
  in
  resolve shared_allergens
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.map snd |> String.concat "," |> print_endline;
  0
