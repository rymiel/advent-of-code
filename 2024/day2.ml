open Aoclib.Util

let day2a i =
  let diffs =
    In_channel.input_lines i
    |> List.map (fun s ->
           String.split_on_char ' ' s |> List.map int_of_string
           |> List.fold_left_map (fun prev i -> (i, prev - i)) 0
           |> snd |> List.tl)
  in
  let safe list =
    (List.for_all (fun n -> n > 0) list || List.for_all (fun n -> n < 0) list)
    && List.for_all (fun n -> abs n >= 1 && abs n <= 3) list
  in
  count_matches safe diffs

let day2b i =
  let lines =
    In_channel.input_lines i
    |> List.map (fun s -> String.split_on_char ' ' s |> List.map int_of_string)
  in
  let diff l =
    l |> List.fold_left_map (fun prev i -> (i, prev - i)) 0 |> snd |> List.tl
  in
  let safe list =
    let list = diff list in
    (List.for_all (fun n -> n > 0) list || List.for_all (fun n -> n < 0) list)
    && List.for_all (fun n -> abs n >= 1 && abs n <= 3) list
  in
  let dampen list =
    fold_lefti
      (fun b _ i ->
        let skipped = List.filteri (fun j _ -> j <> i) list in
        b || safe skipped)
      (safe list) list
  in
  count_matches dampen lines
