open Aoclib.Util

let day3a i =
  let ch = Scanf.Scanning.from_channel i in
  let skip_char ch = Scanf.bscanf ch "%c" (fun _ -> ()) in
  let rec read acc =
    match Scanf.bscanf ch "mul(%d,%d)" ( * ) with
    | x -> read (x :: acc)
    | exception End_of_file -> acc
    | exception Scanf.Scan_failure _ ->
        skip_char ch;
        read acc
  in
  sum (read [])

let day3b i =
  let r = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))\|do()\|don't()|} in
  let s = really_input_string i (in_channel_length i) in
  let acc = ref 0 in
  let doing = ref true in
  let o = ref 0 in
  let rec read s =
    match
      let _ = Str.search_forward r s !o in
      o := Str.match_end ();
      Str.matched_string s
    with
    | "do()" ->
        doing := true;
        read s
    | "don't()" ->
        doing := false;
        read s
    | _ ->
        let a = Str.matched_group 1 s |> int_of_string in
        let b = Str.matched_group 2 s |> int_of_string in
        if !doing then acc := !acc + (a * b);
        read s
    | exception Not_found -> ()
  in
  read s;
  !acc
