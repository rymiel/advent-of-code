open Aoclib.Util

module NewCircularArray = struct
  let of_list list =
    let max = List.length list in
    let arr = Array.make max (-1) in
    let rec aux = function
      | a :: (b :: _ as n) ->
          arr.(a - 1) <- b;
          aux n
      | a :: [] -> arr.(a - 1) <- List.hd list
      | [] -> ()
    in
    aux list;
    arr

  let next arr n = arr.(n - 1)
  let set_after arr k v = arr.(k - 1) <- v

  let pick_up arr n =
    let p1 = next arr n in
    let p2 = next arr p1 in
    let p3 = next arr p2 in
    (p1, p2, p3)

  let destination arr n (p1, p2, p3) =
    let rec aux n =
      if n = 0 then aux (Array.length arr)
      else if n = p1 || n = p2 || n = p3 then aux (n - 1)
      else n
    in
    aux (n - 1)

  let do_move (arr, current) =
    let p1, p2, p3 = pick_up arr current in
    let dest = destination arr current (p1, p2, p3) in
    let after_dest = next arr dest in
    let after_p3 = next arr p3 in
    set_after arr current after_p3;
    set_after arr dest p1;
    set_after arr p3 after_dest;
    (arr, next arr current)

  let order_part1 arr =
    Seq.iterate (next arr) 1
    |> Seq.drop 1
    |> Seq.take (Array.length arr - 1)
    |> Seq.map string_of_int |> List.of_seq |> String.concat "" |> int_of_string

  let product_part2 arr =
    let a = next arr 1 in
    let b = next arr a in
    a * b
end

let parse i =
  In_channel.input_all i |> String.trim |> chars |> List.map digit_of_char

let day23a i =
  let input = parse i in
  let narr = NewCircularArray.of_list input in
  Seq.iterate NewCircularArray.do_move (narr, List.hd input)
  |> Seq.drop 100 |> head |> fst |> NewCircularArray.order_part1

let day23b i =
  let input = parse i in
  let rest =
    Seq.ints (ListExt.max input + 1)
    |> Seq.take_while (fun i -> i <= 1_000_000)
    |> List.of_seq
  in
  let narr = NewCircularArray.of_list (input @ rest) in
  Seq.iterate NewCircularArray.do_move (narr, List.hd input)
  |> Seq.drop 10_000_000 |> head |> fst |> NewCircularArray.product_part2
