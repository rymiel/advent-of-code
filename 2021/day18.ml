open Aoclib.Util

type element = Open | Close | Num of int

let findi_from_step arr predicate start_index step =
  let n = Array.length arr in
  let rec aux i =
    if i < 0 || i >= n then None
    else if predicate arr.(i) then Some i
    else aux (i + step)
  in
  aux start_index

let of_num = function Num i -> i | Open | Close -> failwith "invalid"
let is_num = function Num _ -> true | Open | Close -> false

let array_increment arr idx v =
  Option.iter (fun i -> arr.(i) <- Num (of_num arr.(i) + v)) idx

let find_depth arr depth =
  let depth = ref depth in
  Array.find_index
    (function
      | Open ->
          decr depth;
          false
      | Close ->
          incr depth;
          false
      | Num _ -> !depth = 0)
    arr

let explode_at arr left_index =
  let left = of_num arr.(left_index) in
  let right = of_num arr.(left_index + 1) in
  let left_left_search = findi_from_step arr is_num (left_index - 1) (-1) in
  let right_right_search =
    findi_from_step arr is_num (left_index + 2) 1 |> Option.map (fun i -> i - 3)
  in
  let new_arr = Array.make (Array.length arr - 3) (Num (-1)) in
  Array.blit arr 0 new_arr 0 left_index;
  Array.blit arr (left_index + 3) new_arr left_index
    (Array.length arr - left_index - 3);
  new_arr.(left_index - 1) <- Num 0;
  array_increment new_arr left_left_search left;
  array_increment new_arr right_right_search right;
  new_arr

let try_explode arr = find_depth arr 5 |> Option.map (explode_at arr)

let find_greater_than arr value =
  Array.find_index (function Open | Close -> false | Num i -> i > value) arr

let split_at arr index =
  let value = of_num arr.(index) in
  let left_half = value / 2 in
  let right_half =
    if left_half * 2 <> value then left_half + 1 else left_half
  in
  let new_arr = Array.make (Array.length arr + 3) (Num (-1)) in
  Array.blit arr 0 new_arr 0 index;
  Array.blit arr (index + 1) new_arr (index + 4) (Array.length arr - index - 1);
  Array.blit [| Open; Num left_half; Num right_half; Close |] 0 new_arr index 4;
  new_arr

let try_split arr = find_greater_than arr 9 |> Option.map (split_at arr)

let reduce arr =
  Seq.iterate
    (fun x ->
      match try_explode x with
      | None -> ( match try_split x with None -> x | Some split -> split)
      | Some exploded -> exploded)
    arr
  |> sliding_pair
  |> Seq.drop_while (fun (a, b) -> a <> b)
  |> head |> snd

let add arr1 arr2 =
  let new_arr =
    Array.make (Array.length arr1 + Array.length arr2 + 2) (Num (-1))
  in
  new_arr.(0) <- Open;
  new_arr.(Array.length new_arr - 1) <- Close;
  Array.blit arr1 0 new_arr 1 (Array.length arr1);
  Array.blit arr2 0 new_arr (Array.length arr1 + 1) (Array.length arr2);
  new_arr

let magnitude arr =
  let dispenser = arr |> Array.to_seq |> Seq.to_dispenser in
  let rec aux () =
    match dispenser () with
    | Some Open ->
        let left = aux () in
        let right = aux () in
        assert (dispenser () = Some Close);
        (left * 3) + (right * 2)
    | Some (Num i) -> i
    | _ -> failwith "invalid"
  in
  aux ()

let read_line line =
  String.trim line |> String.to_seq
  |> Seq.filter_map (function
       | '[' -> Some Open
       | ']' -> Some Close
       | '0' .. '9' as c -> Some (Num (digit_of_char c))
       | ',' -> None
       | _ -> failwith "invalid")
  |> Array.of_seq

let parse i = In_channel.input_lines i |> List.map read_line

let day18a i =
  let input = parse i in
  let sum = ListExt.fold_left' (fun acc a -> add acc a |> reduce) input in
  magnitude sum

let day18b i =
  let input = parse i |> List.to_seq in
  let add_magnitude a b = add a b |> reduce |> magnitude in
  Seq.product input input
  |> Seq.map (fun (a, b) -> if a = b then 0 else add_magnitude a b)
  |> SeqExt.max
