open Aoclib
open Aoclib.Util

let rec permute l n f =
  match (n, l) with
  | 1, _ -> List.iter (fun x -> f [ x ]) l
  | _, x :: xs ->
      permute xs (n - 1) (fun ys -> f (x :: ys));
      permute xs n f
  | _, [] -> ()

exception ExitWith of int

let day1a (i : in_channel) : int =
  let nums = In_channel.input_lines i |> List.map int_of_string in
  try
    permute nums 2 (fun x ->
        if Util.sum x = 2020 then raise (ExitWith (Util.product x)));
    failwith "no 2020"
  with ExitWith m -> m

let day1b (i : in_channel) : int =
  let nums = In_channel.input_lines i |> List.map int_of_string in
  try
    permute nums 3 (fun x ->
        if Util.sum x = 2020 then raise (ExitWith (Util.product x)));
    failwith "no 2020"
  with ExitWith m -> m

let day2a (i : in_channel) : int =
  let data = Util.scan_lines i "%d-%d %c: %s%!" (fun a b c d -> (a, b, c, d)) in
  let matches =
    Util.count_matches
      (fun (a, b, c, d) ->
        let m = Util.count_char c d in
        m >= a && m <= b)
      data
  in
  matches

let day2b (i : in_channel) : int =
  let data = Util.scan_lines i "%d-%d %c: %s%!" (fun a b c d -> (a, b, c, d)) in
  let matches =
    Util.count_matches
      (fun (a, b, c, d) -> d.[a - 1] = c <> (d.[b - 1] = c))
      data
  in
  matches

let day3a i =
  let map = Util.BinaryMap2D.read_matrix i '#' in
  let ys = List.init map.height (fun i -> i) in
  Util.count_matches
    (fun y ->
      let x = y * 3 mod map.width in
      Util.BinaryMap2D.has (x, y) map)
    ys

let day3b i =
  let map = Util.BinaryMap2D.read_matrix i '#' in
  let ys = List.init map.height (fun i -> i) in
  let xs = List.init ((map.height / 2) + 1) (fun i -> i) in
  let has_mod (x, y) =
    Util.BinaryMap2D.has (x mod map.width, y mod map.height) map
  in
  let slopes =
    [
      (fun y -> (y, y));
      (fun y -> (y * 3, y));
      (fun y -> (y * 5, y));
      (fun y -> (y * 7, y));
    ]
  in
  let trees =
    List.map (fun fn -> Util.count_matches (fun y -> has_mod (fn y)) ys) slopes
  in
  let trees_x = Util.count_matches (fun x -> has_mod (x, x * 2)) xs in
  trees_x * Util.product trees

let day4a i =
  let required = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ] in
  In_channel.input_all i
  |> Str.split (Str.regexp_string "\n\n")
  |> List.map (Str.global_replace (Str.regexp_string "\n") " ")
  |> List.map (fun s ->
         String.split_on_char ' ' s
         |> List.map (fun e -> String.split_on_char ':' e |> List.hd))
  |> Util.count_matches (fun l -> List.for_all (fun i -> List.mem i l) required)

module StringMap = Map.Make (String)

let day4b i =
  let required = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ] in
  let validate_in_range lo hi s =
    let i = int_of_string s in
    i >= lo && i <= hi
  in
  let is_num = function '0' .. '9' -> true | _ -> false in
  let is_hex = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false in
  let validate_hcl s =
    String.length s = 7
    && s.[0] = '#'
    && String.sub s 1 6 |> Util.chars |> List.for_all is_hex
  in
  let eye_colors = [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ] in
  let validate_ecl s = List.mem s eye_colors in
  let validate_hgt s =
    let num, unit = Scanf.sscanf s "%d%s" (fun a b -> (a, b)) in
    match unit with
    | "cm" -> num >= 150 && num <= 193
    | "in" -> num >= 59 && num <= 76
    | _ -> false
  in
  let validate_pid s =
    String.length s = 9 && Util.chars s |> List.for_all is_num
  in
  let validators =
    StringMap.(
      empty
      |> add "cid" (fun _ -> true)
      |> add "byr" (validate_in_range 1920 2002)
      |> add "iyr" (validate_in_range 2010 2020)
      |> add "eyr" (validate_in_range 2020 2030)
      |> add "hcl" validate_hcl |> add "ecl" validate_ecl
      |> add "hgt" validate_hgt |> add "pid" validate_pid)
  in
  let data =
    In_channel.input_all i
    |> Str.split (Str.regexp_string "\n\n")
    |> List.map (Str.global_replace (Str.regexp_string "\n") " ")
    |> List.map (fun s ->
           String.split_on_char ' ' s
           |> List.map (fun e -> String.split_on_char ':' e |> Util.as_pair))
  in
  let valid passport =
    let keys, _ = List.split passport in
    let has_required = List.for_all (fun i -> List.mem i keys) required in
    let validated =
      List.map (fun (k, v) -> v |> StringMap.find k validators) passport
    in
    has_required && List.for_all (fun x -> x) validated
  in
  Util.count_matches valid data

let day13a i =
  let now = input_line i |> int_of_string in
  let ids =
    input_line i |> String.split_on_char ','
    |> List.filter (( <> ) "x")
    |> List.map int_of_string
  in
  let waits = List.map (fun x -> (x, x - (now mod x))) ids in
  let _, least =
    List.fold_left
      (fun (a, x) (id, wait) -> if wait < a then (wait, id * wait) else (a, x))
      (Int.max_int, 0) waits
  in
  Printf.printf "%d %s %d\n" now
    (String.concat ", "
       (List.map (fun (a, b) -> Printf.sprintf "%d, %d" a b) waits))
    least;
  least

module Day15 = struct
  module IntMap = Map.Make (Int)
  module IntHash = Hashtbl.Make (Int)

  let day15a i =
    let input =
      In_channel.input_all i |> String.split_on_char ','
      |> List.map int_of_string
    in
    let (state : (int * int) IntMap.t ref) = ref IntMap.empty in

    let last = ref (-1) in
    let turn = ref 0 in
    let cycle (a, _) x = (x, a) in
    let speak num =
      incr turn;
      let v = IntMap.find_opt num !state |> Option.value ~default:(0, 0) in
      state := IntMap.add num (cycle v !turn) !state;
      last := num;
      Printf.printf "%4d. <%d>\n" !turn num
    in
    let next () =
      let a, b = IntMap.find !last !state in
      if b = 0 then 0 else a - b
    in
    List.iter speak input;
    while !turn < 2020 do
      next () |> speak
    done;
    !last

  let day15b i =
    let input =
      In_channel.input_all i |> String.split_on_char ','
      |> List.map int_of_string
    in
    let (state : (int * int) IntHash.t) = IntHash.create 5_000_000 in
    let last = ref (-1) in
    let turn = ref 0 in
    let cycle (a, _) x = (x, a) in
    let speak num =
      incr turn;
      let v = IntHash.find_opt state num |> Option.value ~default:(0, 0) in
      IntHash.replace state num (cycle v !turn);
      last := num
    in
    let next () =
      let a, b = IntHash.find state !last in
      if b = 0 then 0 else a - b
    in
    List.iter speak input;
    while !turn < 30000000 do
      next () |> speak
    done;
    !last
end

let () =
  let problem = Sys.argv.(1) in
  let variant = Sys.argv.(2) in
  let solver : solver =
    match (problem, variant) with
    | "day1", "a" -> day1a
    | "day1", "b" -> day1b
    | "day2", "a" -> day2a
    | "day2", "b" -> day2b
    | "day3", "a" -> day3a
    | "day3", "b" -> day3b
    | "day4", "a" -> day4a
    | "day4", "b" -> day4b
    | "day13", "a" -> day13a
    | "day13", "b" -> failwith "todo"
    | "day15", "a" -> Day15.day15a
    | "day15", "b" -> Day15.day15b
    | _ -> failwith (Printf.sprintf "Unknown problem %s %s" problem variant)
  in
  solve 2020 solver problem
