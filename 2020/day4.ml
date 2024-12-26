open Aoclib.Util

let day4a i =
  let required = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ] in
  In_channel.input_lines i |> partition_all ""
  |> List.map (String.concat " ")
  |> List.map (fun s ->
         String.split_on_char ' ' s
         |> List.map (fun e -> String.split_on_char ':' e |> List.hd))
  |> count_matches (fun l -> List.for_all (fun i -> List.mem i l) required)

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
    && String.sub s 1 6 |> chars |> List.for_all is_hex
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
  let validate_pid s = String.length s = 9 && chars s |> List.for_all is_num in
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
    In_channel.input_lines i |> partition_all ""
    |> List.map (String.concat " ")
    |> List.map (fun s ->
           String.split_on_char ' ' s
           |> List.map (fun e -> String.split_on_char ':' e |> as_pair))
  in
  let valid passport =
    let keys, _ = List.split passport in
    let has_required = List.for_all (fun i -> List.mem i keys) required in
    let validated =
      List.map (fun (k, v) -> v |> StringMap.find k validators) passport
    in
    has_required && List.for_all (fun x -> x) validated
  in
  count_matches valid data
