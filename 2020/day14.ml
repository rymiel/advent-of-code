open Aoclib.Util

let read_masks (s : string) =
  String.fold_left
    (fun (on, off) c ->
      let on = on lsl 1 in
      let off = off lsl 1 in
      match c with
      | 'X' -> (on, off + 1)
      | '1' -> (on + 1, off + 1)
      | '0' -> (on, off)
      | _ -> failwith "invalid")
    (0, 0) s

let apply_masks (on, off) i = i lor on land off

let day14a i =
  let memory = Hashtbl.create 1_000_000 in
  In_channel.input_lines i
  |> List.fold_left
       (fun masks line ->
         let lhs, rhs =
           line |> String.split_on_char '=' |> List.map String.trim |> as_pair
         in
         if lhs = "mask" then read_masks rhs
         else
           let address = Scanf.sscanf lhs "mem[%d]" Fun.id in
           let value = rhs |> int_of_string |> apply_masks masks in
           Hashtbl.replace memory address value;
           masks)
       (0, 0)
  |> ignore;
  Hashtbl.to_seq_values memory |> SeqExt.sum

let shift_masks c (on, off) =
  let on = on lsl 1 in
  let off = off lsl 1 in
  match c with
  | 'X' -> [ (on, off); (on + 1, off + 1) ]
  | '1' -> [ (on + 1, off + 1) ]
  | '0' -> [ (on, off + 1) ]
  | _ -> failwith "invalid"

let read_floating_masks (s : string) =
  String.fold_left (fun ms c -> List.concat_map (shift_masks c) ms) [ (0, 0) ] s

let day14b i =
  let memory = Hashtbl.create 1_000_000 in
  In_channel.input_lines i
  |> List.fold_left
       (fun masks line ->
         let lhs, rhs =
           line |> String.split_on_char '=' |> List.map String.trim |> as_pair
         in
         if lhs = "mask" then read_floating_masks rhs
         else
           let address = Scanf.sscanf lhs "mem[%d]" Fun.id in
           let value = rhs |> int_of_string in
           List.iter
             (fun mask ->
               Hashtbl.replace memory (apply_masks mask address) value)
             masks;
           masks)
       [ (0, 0) ]
  |> ignore;
  Hashtbl.to_seq_values memory |> SeqExt.sum
