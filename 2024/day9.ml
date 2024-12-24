open Aoclib.Util

let day9a i =
  let nums =
    In_channel.input_all i |> String.trim |> chars_s |> List.map int_of_string
  in
  let arr = Array.make (sum nums) (-1) in
  let rec create_file index file_id = function
    | size :: xs ->
        Array.fill arr index size file_id;
        create_gap (index + size) file_id xs
    | [] -> ()
  and create_gap index file_id = function
    | size :: xs ->
        Array.fill arr index size (-1);
        create_file (index + size) (file_id + 1) xs
    | [] -> ()
  in
  create_file 0 0 nums;
  let find_index_last p a =
    let rec loop i =
      if i = -1 then failwith "find_index_last"
      else if p a.(i) then i
      else loop (pred i)
    in
    loop (Array.length a - 1)
  in
  let find_index_first p a =
    match Array.find_index p a with
    | Some x -> x
    | None -> failwith "find_index_first"
  in
  let rec swap () =
    let hole = find_index_first (( = ) (-1)) arr in
    let file = find_index_last (( <> ) (-1)) arr in
    if hole <= file then (
      arr.(hole) <- arr.(file);
      arr.(file) <- -1;
      swap ())
  in
  let checksum arr =
    Array.to_seq arr
    |> Seq.filter (( <> ) (-1))
    |> Seq.fold_lefti (fun acc i e -> acc + (i * e)) 0
  in
  swap ();
  checksum arr

let day9b i =
  let nums =
    In_channel.input_all i |> String.trim |> chars_s |> List.map int_of_string
  in
  let rec create_file file_id = function
    | size :: xs ->
        let n = (Some file_id, size) in
        n :: create_gap file_id xs
    | [] -> []
  and create_gap file_id = function
    | size :: xs ->
        let n = (None, size) in
        n :: create_file (file_id + 1) xs
    | [] -> []
  in
  let fs = create_file 0 nums |> ref in
  let swap (fb, flen) =
    match
      List.find_mapi
        (fun idx (b, len) ->
          match b with
          | Some x -> if x = fb then Some None else None
          | None -> if len >= flen then Some (Some (idx, len - flen)) else None)
        !fs
    with
    | Some (Some (idx, nlen)) ->
        fs :=
          List.to_seq !fs
          |> Seq.mapi (fun i a -> (i, a))
          |> Seq.flat_map (fun (i, (b, len)) ->
                 if i = idx then List.to_seq [ (Some fb, flen); (None, nlen) ]
                 else if b = Some fb then Seq.return (None, flen)
                 else Seq.return (b, len))
          |> List.of_seq
    | _ -> ()
  in
  let move_all () =
    !fs |> List.rev
    |> List.filter_map (fun (b, i) ->
           match b with Some x -> Some (x, i) | None -> None)
    |> List.iter swap
  in
  let checksum (fs : (int option * int) list) =
    List.to_seq fs
    |> Seq.map (fun (b, i) -> match b with None -> (0, i) | Some x -> (x, i))
    |> Seq.fold_left
         (fun (acc, idx) (block, length) ->
           ( acc
             + (Seq.ints idx |> Seq.take length
               |> Seq.fold_left (fun acc i -> acc + (i * block)) 0),
             idx + length ))
         (0, 0)
    |> fst
  in
  move_all ();
  checksum !fs
