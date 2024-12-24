open Aoclib
open Aoclib.Util

let find_in_keypad (kp : char list list) needle =
  List.find_mapi
    (fun y ->
      List.find_mapi (fun x c -> if c = needle then Some (x, y) else None))
    kp
  |> Option.get

let num_kp =
  [ [ '7'; '8'; '9' ]; [ '4'; '5'; '6' ]; [ '1'; '2'; '3' ]; [ ' '; '0'; 'A' ] ]

let arrow_kp = [ [ ' '; '^'; 'A' ]; [ '<'; 'v'; '>' ] ]
let initial kp = find_in_keypad kp 'A'
let dead_spot kp = find_in_keypad kp ' '

let horizontal n =
  if n = 0 then [||]
  else if n > 0 then Array.make n '>'
  else Array.make (abs n) '<'

let vertical n =
  if n = 0 then [||]
  else if n > 0 then Array.make n 'v'
  else Array.make (abs n) '^'

let navigate start goal dead_spot =
  let dx, dy = Coord.(goal - start) in

  let vfirst = Array.concat [ vertical dy; horizontal dx; [| 'A' |] ] in
  let vpivot = Coord.(start + (0, dy)) in

  let hfirst = Array.concat [ horizontal dx; vertical dy; [| 'A' |] ] in
  let hpivot = Coord.(start + (dx, 0)) in

  let path =
    if vpivot = dead_spot then [ hfirst ]
    else if hpivot = dead_spot then [ vfirst ]
    else if Array.for_all2 ( = ) vfirst hfirst then [ vfirst ]
    else [ vfirst; hfirst ]
  in
  path

let day21a i =
  let codes = In_channel.input_lines i in

  let trace kp code =
    let i = initial kp in
    let dead = dead_spot kp in
    Seq.fold_left
      (fun (a, p) t ->
        let n = find_in_keypad kp t in
        let paths = navigate p n dead in
        (List.concat_map (fun e -> List.map (Array.append e) paths) a, n))
      ([ [||] ], i) code
    |> fst
  in
  let complexity code =
    let p0 = trace num_kp (code |> String.to_seq) in
    let p1 = List.concat_map (fun p -> trace arrow_kp (Array.to_seq p)) p0 in
    let p2 = List.concat_map (fun p -> trace arrow_kp (Array.to_seq p)) p1 in

    let len = p2 |> List.map Array.length |> List.fold_left min Int.max_int in
    let numeric_code = String.sub code 0 3 |> int_of_string in
    len * numeric_code
  in
  List.map complexity codes |> sum

let day21b i =
  let codes = In_channel.input_lines i in
  let min_elt = function
    | [] -> failwith "min_elt empty"
    | x :: xs -> List.fold_left min x xs
  in
  let cache = Hashtbl.create 1_000_000 in
  let rec trace depth target code =
    let kp = if depth = 0 then num_kp else arrow_kp in
    let init = initial kp in
    let dead = dead_spot kp in
    let aux () =
      String.fold_left
        (fun (r, pos) char ->
          let next = find_in_keypad kp char in
          let paths = navigate pos next dead in
          let nested =
            if depth = target then paths |> List.map Array.length
            else
              List.map
                (fun path ->
                  trace (depth + 1) target (Array.to_seq path |> String.of_seq))
                paths
          in
          (r + min_elt nested, next))
        (0, init) code
      |> fst
    in
    match Hashtbl.find_opt cache (code, depth) with
    | Some x -> x
    | None ->
        let r = aux () in
        Hashtbl.replace cache (code, depth) r;
        r
  in
  let complexity code =
    let len = trace 0 25 code in
    let numeric_code = String.sub code 0 3 |> int_of_string in
    len * numeric_code
  in
  List.map complexity codes |> sum
