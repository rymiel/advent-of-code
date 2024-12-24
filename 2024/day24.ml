open Aoclib.Util

type op = And | Or | Xor
type gate = { left : string; right : string; op : op; out : string }

let do_op = function And -> ( && ) | Or -> ( || ) | Xor -> ( <> )

let parse i =
  let initial, gates = partition_at "" (In_channel.input_lines i) in
  let initial =
    List.map
      (fun s ->
        Scanf.sscanf s "%s@: %d" (fun k v -> (k, if v = 1 then true else false)))
      initial
  in
  let gates =
    List.map
      (fun s ->
        Scanf.sscanf s "%s %s %s -> %s" (fun left op right out ->
            let op =
              match op with
              | "AND" -> And
              | "OR" -> Or
              | "XOR" -> Xor
              | _ -> failwith "invalid op"
            in
            { left; right; op; out }))
      gates
  in
  (initial, gates)

let day24a i =
  let initial, gates = parse i in
  let state = Hashtbl.create 1_000_000 in
  let affected = Hashtbl.create 1_000_000 in

  List.iter
    (fun g ->
      Hashtbl.add affected g.left g;
      Hashtbl.add affected g.right g)
    gates;
  List.iter (fun (k, v) -> Hashtbl.replace state k v) initial;

  let rec propagate signal =
    Hashtbl.find_all affected signal
    |> List.iter (fun gate ->
           let left = Hashtbl.find_opt state gate.left in
           let right = Hashtbl.find_opt state gate.right in
           match (left, right) with
           | Some l, Some r ->
               let result = do_op gate.op l r in
               Hashtbl.replace state gate.out result;
               propagate gate.out
           | _ -> ())
  in

  List.iter (fun (k, _) -> propagate k) initial;

  Hashtbl.to_seq state
  |> Seq.filter (fun (k, _) -> k.[0] = 'z')
  |> List.of_seq |> List.sort compare
  |> List.fold_left
       (fun (i, mul) (_, b) -> ((i + if b then mul else 0), mul lsl 1))
       (0, 1)
  |> fst

exception RecoveryError of (string * string)

let day24b i =
  let _, gates = parse i in

  let has_gate gates g =
    List.mem g gates || List.mem { g with left = g.right; right = g.left } gates
  in

  let find_out gates left right op =
    List.find_opt
      (fun g ->
        ((g.left = left && g.right = right)
        || (g.left = right && g.right = left))
        && g.op = op)
      gates
    |> Option.map (fun g -> g.out)
  in

  let find_reverse gates i op out =
    List.find_opt
      (fun g -> (g.left = i || g.right = i) && g.op = op && g.out = out)
      gates
    |> Option.map (fun g -> if g.left = i then g.right else g.left)
  in

  let rec verify_adder width gates swaps =
    let carry = ref None in
    let rec aux bit =
      let x = Printf.sprintf "x%02d" bit in
      let y = Printf.sprintf "y%02d" bit in
      let z = Printf.sprintf "z%02d" bit in
      if bit = 0 then
        (* half adder
           X XOR Y -> Z
           X AND Y -> C(+1) *)
        has_gate gates { left = x; right = y; op = Xor; out = z }
        && (carry := find_out gates x y And;
            Option.is_some !carry)
        && aux 1
      else if bit > width then
        match !carry with Some c -> c = z | None -> false
      else
        (* full adder
           X XOR Y -> i
           X AND Y -> j
           i AND C(-1) -> f
           i XOR C(-1) -> Z
           j OR f -> C(+1) *)
        let io = find_out gates x y Xor in
        let jo = find_out gates x y And in
        match (!carry, io) with
        | Some c, Some i -> (
            let fo = find_out gates i c And in
            let hz =
              has_gate gates { left = i; right = c; op = Xor; out = z }
            in
            (if Option.is_none fo || not hz then
               (* recover: find z output XOR node with one matching input; swap the mismatching input with that
                  one's other input *)
               let recoveri =
                 find_reverse gates i Xor z |> Option.map (fun x -> (x, c))
               in
               let recoverc =
                 find_reverse gates c Xor z |> Option.map (fun x -> (x, i))
               in
               Seq.append (Option.to_seq recoveri) (Option.to_seq recoverc)
               |> Seq.iter (fun r -> raise (RecoveryError r)));
            if not hz then
              (* recover: find current output of XOR operation; swap with the expected z output *)
              find_out gates i c Xor
              |> Option.iter (fun sz -> raise (RecoveryError (sz, z)));
            hz
            &&
            match (jo, fo) with
            | Some j, Some f ->
                (carry := find_out gates j f Or;
                 Option.is_some !carry)
                && aux (bit + 1)
            | _ -> false)
        | _ -> false
    in
    match aux 0 with
    | true -> Some swaps
    | false -> None
    | exception RecoveryError (a, b) ->
        let swapped =
          List.map
            (fun g ->
              if g.out = a then { g with out = b }
              else if g.out = b then { g with out = a }
              else g)
            gates
        in
        verify_adder width swapped ((a, b) :: swaps)
  in

  let bits =
    gates
    |> List.concat_map (fun g -> [ g.left; g.right ])
    |> List.filter_map (fun n ->
           if n.[0] = 'x' then
             String.sub n 1 (String.length n - 1) |> int_of_string_opt
           else None)
    |> List.fold_left max 0
  in

  let result = verify_adder bits gates [] in
  (match result with
  | Some x ->
      x
      |> List.concat_map (fun (a, b) -> [ a; b ])
      |> List.sort compare |> String.concat "," |> print_endline
  | None -> print_endline "not found");
  0
