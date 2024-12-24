open Aoclib
open Aoclib.Util

let day14a i =
  let input =
    scan_lines i "p=%d,%d v=%d,%d" (fun px py vx vy -> ((px, py), (vx, vy)))
  in
  (* let width = 11 in
     let height = 7 in *)
  let width = 101 in
  let height = 103 in
  let end_positions =
    List.map (fun (p, v) -> Coord.((p + (v * 100)) % (width, height))) input
  in
  let quadrant (x, y) =
    let mid_width = width / 2 in
    let mid_height = height / 2 in
    if x < mid_width && y < mid_height then Some 0
    else if x > mid_width && y < mid_height then Some 1
    else if x < mid_width && y > mid_height then Some 2
    else if x > mid_width && y > mid_height then Some 3
    else None
  in
  let partitioned =
    List.fold_left
      (fun q p ->
        match quadrant p with
        | Some i ->
            q.(i) <- q.(i) + 1;
            q
        | None -> q)
      [| 0; 0; 0; 0 |] end_positions
  in
  Array.to_list partitioned |> product

let day14b i =
  let input =
    scan_lines i "p=%d,%d v=%d,%d" (fun px py vx vy -> ((px, py), (vx, vy)))
  in
  let width = 101 in
  let height = 103 in
  let positions n =
    let table = Hashtbl.create 1000 in
    List.iter
      (fun (p, v) ->
        Hashtbl.add table Coord.((p + (v * n)) % (width, height)) ())
      input;
    table
  in
  let draw table =
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        print_char (if Hashtbl.mem table (x, y) then '#' else ' ')
      done;
      print_newline ()
    done
  in
  let directions =
    [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]
  in
  let winner =
    Seq.ints 0
    |> Seq.find (fun n ->
           let table = positions n in
           Hashtbl.to_seq_keys table
           |> Seq.exists (fun p ->
                  List.for_all
                    (fun d -> Coord.(p + d) |> Hashtbl.mem table)
                    directions))
    |> Option.get
  in
  draw (positions winner);
  winner
