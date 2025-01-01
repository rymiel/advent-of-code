open Aoclib.Util

module CircularArray = struct
  let of_list = Array.of_list

  let to_string arr =
    Array.map string_of_int arr
    |> Array.to_list |> String.concat " " |> Printf.sprintf "[%s]"

  let get arr n = Array.get arr (n mod Array.length arr)
  let set arr n = Array.set arr (n mod Array.length arr)

  let blit src src_pos dst dst_pos len =
    for i = 0 to len - 1 do
      set dst (dst_pos + i) (get src (src_pos + i))
    done

  let pick_up arr n =
    let buf = Array.make 3 0 in
    blit arr (n + 1) buf 0 3;
    buf

  let destination_label arr n picked_up =
    let rec aux n =
      if n = 0 then aux 9 else if Array.mem n picked_up then aux (n - 1) else n
    in
    aux (get arr n - 1)

  let find_index arr n e =
    let t = Array.length arr in
    let rec loop i =
      if i = t then failwith "not found"
      else if get arr (n + i) = e then n + i
      else loop (succ i)
    in
    loop 0

  let do_move arr n =
    let picked_up = pick_up arr n in
    let dest_label = destination_label arr n picked_up in
    let dest_idx = find_index arr n dest_label in
    let move_n = dest_idx - (n + 3) in
    blit arr (n + 4) arr (n + 1) move_n;
    blit picked_up 0 arr (n + 1 + move_n) 3;
    arr

  let order arr =
    let one = find_index arr 0 1 in
    Seq.ints 1 |> Seq.take 8
    |> Seq.map (fun i -> get arr (i + one))
    |> Seq.map string_of_int |> List.of_seq |> String.concat "" |> int_of_string

  let iterate (arr, n) = (do_move arr n, n + 1)
end

let day23a i =
  let arr =
    In_channel.input_all i |> String.trim |> chars |> List.map digit_of_char
    |> CircularArray.of_list
  in
  Seq.iterate CircularArray.iterate (arr, 0)
  |> Seq.drop 100 |> head |> fst |> CircularArray.order

let day23b i =
  ignore i;
  failwith "todo"
