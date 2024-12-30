open Day13ex.Data
open Day13ex
open Aoclib.Util

let rec compare_nodes a b =
  match (a, b) with
  | Num ia, Num ib -> Int.compare ia ib
  | Num _, List lb -> compare_lists [ a ] lb
  | List la, Num _ -> compare_lists la [ b ]
  | List la, List lb -> compare_lists la lb

and compare_lists la lb = List.compare compare_nodes la lb

let parse i =
  let p = Lexing.from_channel i in
  Parser.main Lexer.token p

let day13a i =
  parse i
  |> List.mapi (fun i (a, b) -> if compare_nodes a b = -1 then i + 1 else 0)
  |> ListExt.sum

let day13b i =
  let divider0 = List [ List [ Num 2 ] ] in
  let divider1 = List [ List [ Num 6 ] ] in
  let order =
    parse i
    |> List.concat_map (fun (a, b) -> [ a; b ])
    |> List.append [ divider0; divider1 ]
    |> List.sort compare_nodes
  in
  let find i = List.find_index (( = ) i) order |> Option.get |> ( + ) 1 in
  find divider0 * find divider1
