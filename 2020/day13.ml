let day13a i =
  let now = input_line i |> int_of_string in
  let ids =
    input_line i |> String.split_on_char ','
    |> List.filter (( <> ) "x")
    |> List.map int_of_string
  in
  let waits = List.map (fun x -> (x, x - (now mod x))) ids in
  let _, least =
    List.fold_left
      (fun (a, x) (id, wait) -> if wait < a then (wait, id * wait) else (a, x))
      (Int.max_int, 0) waits
  in
  Printf.printf "%d %s %d\n" now
    (String.concat ", "
       (List.map (fun (a, b) -> Printf.sprintf "%d, %d" a b) waits))
    least;
  least
