open Day19ex
open Aoclib.Util

let skip s n = String.sub s n (String.length s - n)

let parse i =
  let rules, input = In_channel.input_lines i |> partition_at "" in
  let table = Hashtbl.create 0 in
  List.iter
    (fun s ->
      let i, e = Parser.rule Lexer.token (Lexing.from_string s) in
      Hashtbl.replace table i e)
    rules;
  (table, input)

let day19a i =
  let table, input = parse i in
  let rule = Hashtbl.find table in
  let rec match_rule expr input =
    match expr with
    | Data.Chr c ->
        if String.length input = 0 then None
        else if input.[0] = c then Some (skip input 1)
        else None
    | Data.List l -> match_list l input
    | Data.Alternate (a, b) ->
        let ra = match_list a input in
        let rb = match_list b input in
        Option.fold ~none:rb ~some:Option.some ra
  and match_list list input =
    List.fold_left
      (fun o r -> Option.bind o (match_rule (rule r)))
      (Some input) list
  in
  let matches_rule expr input =
    match match_rule expr input with Some "" -> true | Some _ | None -> false
  in
  ListExt.count_matches (matches_rule (rule 0)) input

let day19b i =
  let table, input = parse i in
  (* 8: 42 | 42 8
     11: 42 31 | 42 11 31 *)
  Hashtbl.replace table 8 (Alternate ([ 42 ], [ 42; 8 ]));
  Hashtbl.replace table 11 (Alternate ([ 42; 31 ], [ 42; 11; 31 ]));
  let rule = Hashtbl.find table in
  let rec match_rule expr input =
    match expr with
    | Data.Chr c ->
        if String.length input = 0 then Seq.empty
        else if input.[0] = c then Seq.return (skip input 1)
        else Seq.empty
    | Data.List l -> match_list l input
    | Data.Alternate (a, b) ->
        let ra = match_list a input in
        let rb = match_list b input in
        Seq.append ra rb
  and match_list list input =
    List.fold_left
      (fun seq r -> Seq.flat_map (match_rule (rule r)) seq)
      (Seq.return input) list
  in
  let matches_rule expr input =
    match_rule expr input |> Seq.exists (( = ) "")
  in
  ListExt.count_matches (matches_rule (rule 0)) input
