open Aoc
open Aoc.Util

let parse_day1 i = scan_lines i "%d %d%!" (fun a b -> (a, b)) |> List.split

let day1a (i : in_channel) : int =
  let left, right = parse_day1 i in
  let pairs = List.combine (List.sort compare left) (List.sort compare right) in
  List.fold_left (fun n (a, b) -> n + abs (a - b)) 0 pairs

let day1b i =
  let left, right = parse_day1 i in
  List.fold_left (fun n i -> n + (i * count_item i right)) 0 left

let day2a i =
  let diffs =
    In_channel.input_lines i
    |> List.map (fun s ->
           String.split_on_char ' ' s |> List.map int_of_string
           |> List.fold_left_map (fun prev i -> (i, prev - i)) 0
           |> snd |> List.tl)
  in
  let safe list =
    (List.for_all (fun n -> n > 0) list || List.for_all (fun n -> n < 0) list)
    && List.for_all (fun n -> abs n >= 1 && abs n <= 3) list
  in
  count_matches safe diffs

let day2b i =
  let lines =
    In_channel.input_lines i
    |> List.map (fun s -> String.split_on_char ' ' s |> List.map int_of_string)
  in
  let diff l =
    l |> List.fold_left_map (fun prev i -> (i, prev - i)) 0 |> snd |> List.tl
  in
  let safe list =
    let list = diff list in
    (List.for_all (fun n -> n > 0) list || List.for_all (fun n -> n < 0) list)
    && List.for_all (fun n -> abs n >= 1 && abs n <= 3) list
  in
  let dampen list =
    fold_lefti
      (fun b _ i ->
        let skipped = List.filteri (fun j _ -> j <> i) list in
        b || safe skipped)
      (safe list) list
  in
  count_matches dampen lines

let day3a i =
  let ch = Scanf.Scanning.from_channel i in
  let skip_char ch = Scanf.bscanf ch "%c" (fun _ -> ()) in
  let rec read acc =
    match Scanf.bscanf ch "mul(%d,%d)" ( * ) with
    | x -> read (x :: acc)
    | exception End_of_file -> acc
    | exception Scanf.Scan_failure _ ->
        skip_char ch;
        read acc
  in
  sum (read [])

let day3b i =
  let r = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))\|do()\|don't()|} in
  let s = really_input_string i (in_channel_length i) in
  let acc = ref 0 in
  let doing = ref true in
  let o = ref 0 in
  let rec read s =
    match
      let _ = Str.search_forward r s !o in
      o := Str.match_end ();
      Str.matched_string s
    with
    | "do()" ->
        print_endline "do()";
        doing := true;
        read s
    | "don't()" ->
        print_endline "don't()";
        doing := false;
        read s
    | _ ->
        let a = Str.matched_group 1 s |> int_of_string in
        let b = Str.matched_group 2 s |> int_of_string in
        Printf.printf "(%d, %d)\n%!" a b;
        if !doing then acc := !acc + (a * b);
        read s
    | exception Not_found ->
        print_endline "not found";
        ()
  in
  read s;
  !acc

let parse_day4 i =
  let lines = In_channel.input_lines i in
  let add_row y m s =
    Util.fold_lefti (fun m c x -> CoordMap.add (x, y) c m) m (Util.chars s)
  in
  Util.fold_lefti (fun m line y -> add_row y m line) CoordMap.empty lines

let day4a i =
  let map = parse_day4 i in
  let letters = [ (0, 'X'); (1, 'M'); (2, 'A'); (3, 'S') ] in
  let has_xmas start delta map =
    List.for_all
      (fun (scalar, letter) ->
        CoordMap.find_opt Coord.(start + (delta * scalar)) map = Some letter)
      letters
  in
  let directions =
    [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]
  in
  let acc = ref 0 in
  CoordMap.iter
    (fun pos c ->
      if c = 'X' then
        List.iter (fun dir -> if has_xmas pos dir map then incr acc) directions)
    map;
  !acc

let day4b i =
  let map = parse_day4 i in
  let directions = [ (-1, -1); (1, -1); (-1, 1); (1, 1) ] in
  let has_xmas start map =
    let corners =
      List.map
        (fun delta -> CoordMap.find_opt Coord.(start + delta) map)
        directions
    in
    let ms =
      List.filter
        (fun delta -> CoordMap.find_opt Coord.(start + delta) map = Some 'M')
        directions
    in
    let result =
      CoordMap.find_opt start map = Some 'A'
      && count_item (Some 'M') corners = 2
      && count_item (Some 'S') corners = 2
      &&
      let m1x, m1y = List.nth ms 0 in
      let m2x, m2y = List.nth ms 1 in
      m1x = m2x || m1y = m2y
    in
    result
  in
  let acc = ref 0 in
  CoordMap.iter (fun pos c -> if c = 'A' && has_xmas pos map then incr acc) map;
  !acc

module Day5 = struct
  let parse i =
    let rules, input = partition_at "" (In_channel.input_lines i) in
    let rules =
      rules
      |> List.map (fun s ->
             String.split_on_char '|' s |> List.map int_of_string |> as_pair)
    in
    let input =
      input
      |> List.map (fun s ->
             String.split_on_char ',' s |> List.map int_of_string)
    in
    (rules, input)

  let check_forward current rest rules =
    not @@ List.exists (fun r -> List.mem (r, current) rules) rest

  let rec correct rules = function
    | [] -> true
    | x :: xs -> check_forward x xs rules && correct rules xs

  let middle_value l = List.nth l (List.length l / 2)

  let day5a i =
    let rules, input = parse i in
    let correct_input = List.filter (correct rules) input in
    List.map middle_value correct_input |> sum

  let day5b i =
    let rules, input = parse i in
    let incorrect_input = List.filter (fun l -> not @@ correct rules l) input in
    let rule_compare a b =
      if List.mem (a, b) rules then -1
      else if List.mem (b, a) rules then 1
      else 0
    in
    let fix = List.stable_sort rule_compare in
    let corrected = List.map fix incorrect_input in
    List.map middle_value corrected |> sum
end

module Day6 = struct
  let rotate (x, y) = (~-y, x)

  let parse_day6 i =
    let start = ref (0, 0) in
    let map =
      BinaryMap2D.read_matrix_cb i (fun c pos ->
          if c = '^' then start := pos;
          c = '#')
    in
    (!start, map)

  let day6a i =
    let start, map = parse_day6 i in
    let current = ref start in
    let facing = ref (0, -1) (* up *) in
    let visited = ref @@ CoordSet.empty in
    let rec move () =
      if BinaryMap2D.oob !current map then ()
      else (
        visited := CoordSet.add !current !visited;
        let next = Coord.(!current + !facing) in
        if BinaryMap2D.has next map then facing := rotate !facing
        else current := next;
        move ())
    in
    move ();
    CoordSet.to_seq !visited |> Seq.length

  let day6b i =
    let start, map = parse_day6 i in
    let visited = Hashtbl.create 2000 in
    let loops_with_obstacle obs =
      let current = ref start in
      let facing = ref (0, -1) (* up *) in
      let map =
        Option.fold ~none:map ~some:(fun o -> BinaryMap2D.add o map) obs
      in
      let visited_facing = Hashtbl.create 2000 in
      Hashtbl.clear visited;
      let rec loops () =
        if BinaryMap2D.oob !current map then false
        else if Hashtbl.mem visited_facing (!current, !facing) then true
        else (
          Hashtbl.replace visited !current ();
          Hashtbl.replace visited_facing (!current, !facing) ();
          let next = Coord.(!current + !facing) in
          if BinaryMap2D.has next map then facing := rotate !facing
          else current := next;
          loops ())
      in
      loops ()
    in
    loops_with_obstacle None |> ignore;
    Hashtbl.remove visited start;
    let path = Hashtbl.to_seq_keys visited |> Array.of_seq in
    let pool = Domainslib.Task.setup_pool ~num_domains:6 () in
    Domainslib.Task.run pool (fun () ->
        Domainslib.Task.parallel_for_reduce ~start:0
          ~finish:(Array.length path - 1)
          ~body:(fun i -> if loops_with_obstacle (Some path.(i)) then 1 else 0)
          pool ( + ) 0)
end

let parse_day7 i =
  In_channel.input_lines i
  |> List.map (fun s ->
         let target, nums =
           String.split_on_char ':' s |> List.map String.trim |> as_pair
         in
         let target = int_of_string target in
         let nums =
           String.split_on_char ' ' nums |> List.map int_of_string |> List.rev
         in
         (target, nums))

let day7a i =
  let data = parse_day7 i in
  let rec operations list =
    match list with
    | x :: [] -> [ x ]
    | x :: xs ->
        let choices = operations xs in
        let adds = List.map (fun n -> x + n) choices in
        let muls = List.map (fun n -> x * n) choices in
        adds @ muls
    | [] -> failwith "empty"
  in
  List.map
    (fun (target, nums) ->
      if List.mem target (operations nums) then target else 0)
    data
  |> sum

let day7b i =
  let data = parse_day7 i in
  let cat a b = string_of_int b ^ string_of_int a |> int_of_string in
  let rec operations list =
    match list with
    | x :: [] -> [| x |]
    | x :: xs ->
        let choices = operations xs in
        [ ( + ); ( * ); cat ]
        |> List.map (fun op -> choices |> Array.map (fun n -> op x n))
        |> Array.concat
    | [] -> failwith "empty"
  in
  List.filter_map
    (fun (target, nums) ->
      if Array.mem target (operations nums) then Some target else None)
    data
  |> sum

let day8a i =
  let map = CharMap2D.read_matrix i '.' in
  let antinodes = Hashtbl.create 100 in
  CharMap2D.iter
    (fun c p ->
      CharMap2D.iter
        (fun c' p' ->
          if c = c' && p <> p' then
            let diff = Coord.(p - p') in
            let antinode = Coord.(p + diff) in
            if CharMap2D.in_bounds antinode map then
              Hashtbl.replace antinodes antinode ())
        map)
    map;
  Hashtbl.length antinodes

let day8b i =
  let map = CharMap2D.read_matrix i '.' in
  let antinodes = Hashtbl.create 100 in
  CharMap2D.iter
    (fun c p ->
      CharMap2D.iter
        (fun c' p' ->
          if c = c' && p <> p' then
            let step = Coord.(p - p') in
            let antinode = ref p in
            while CharMap2D.in_bounds !antinode map do
              Hashtbl.replace antinodes !antinode ();
              antinode := Coord.(!antinode + step)
            done)
        map)
    map;
  Hashtbl.length antinodes

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

let day10a i =
  let lines = In_channel.input_lines i in
  let height = List.length lines in
  let width = List.hd lines |> String.length in
  let table = Hashtbl.create (height * width) in
  let add_row y s =
    List.iteri
      (fun x c -> Hashtbl.add table (x, y) (int_of_string c))
      (Util.chars_s s)
  in
  List.iteri (fun y line -> add_row y line) lines;
  let dirs = [ Coord.up; Coord.down; Coord.left; Coord.right ] in
  let rec nines from =
    let current = Hashtbl.find table from in
    if current = 9 then [ from ]
    else
      List.map (Coord.( + ) from) dirs
      |> List.filter (fun p -> Hashtbl.find_opt table p = Some (current + 1))
      |> List.concat_map nines
  in
  Hashtbl.to_seq table
  |> Seq.filter (fun (_, v) -> v = 0)
  |> Seq.map fst
  |> Seq.map (fun p -> nines p |> List.sort_uniq compare |> List.length)
  |> Seq.fold_left ( + ) 0

let day10b i =
  let lines = In_channel.input_lines i in
  let height = List.length lines in
  let width = List.hd lines |> String.length in
  let table = Hashtbl.create (height * width) in
  let add_row y s =
    List.iteri
      (fun x c -> Hashtbl.add table (x, y) (int_of_string c))
      (Util.chars_s s)
  in
  List.iteri (fun y line -> add_row y line) lines;
  let dirs = [ Coord.up; Coord.down; Coord.left; Coord.right ] in
  let rec nines from =
    let current = Hashtbl.find table from in
    if current = 9 then [ from ]
    else
      List.map (Coord.( + ) from) dirs
      |> List.filter (fun p -> Hashtbl.find_opt table p = Some (current + 1))
      |> List.concat_map nines
  in
  Hashtbl.to_seq table
  |> Seq.filter_map (fun (k, v) ->
         if v = 0 then Some (nines k |> List.length) else None)
  |> Seq.fold_left ( + ) 0

let day11 target i =
  let nums =
    In_channel.input_all i |> String.trim |> String.split_on_char ' '
    |> List.map int_of_string
  in
  let cache = Hashtbl.create 1_000_000 in
  let rec blink n step =
    let aux () =
      if n = 0 then blink 1 (step + 1)
      else
        let s = string_of_int n in
        let len = String.length s in
        if len mod 2 = 0 then
          let half = len / 2 in
          let left = int_of_string (String.sub s 0 half) in
          let right = int_of_string (String.sub s half half) in
          blink left (step + 1) + blink right (step + 1)
        else blink (n * 2024) (step + 1)
    in
    if step = target then 1
    else
      match Hashtbl.find_opt cache (n, step) with
      | Some x -> x
      | None ->
          let r = aux () in
          Hashtbl.replace cache (n, step) r;
          r
  in
  List.map (fun n -> blink n 0) nums |> sum

let day11a = day11 25
let day11b = day11 75

let day12a i =
  let lines = In_channel.input_lines i in
  let height = List.length lines in
  let width = List.hd lines |> String.length in
  let table = Hashtbl.create (height * width) in
  let add_row y s =
    List.iteri (fun x c -> Hashtbl.add table (x, y) c) (Util.chars s)
  in
  List.iteri (fun y line -> add_row y line) lines;
  let remaining = Hashtbl.copy table in
  let dirs = [ (-1, 0); (0, -1); (1, 0); (0, 1) ] in
  let flood_fill (ix, iy) chr =
    let region = Hashtbl.create 100 in
    let rec step p =
      if Hashtbl.mem region p then ()
      else
        match Hashtbl.find_opt table p with
        | Some c when c = chr ->
            Hashtbl.add region p ();
            List.iter (fun d -> step Coord.(p + d)) dirs
        | _ -> ()
    in
    step (ix, iy);
    region
  in
  let area region = Hashtbl.length region in
  let perimeter region =
    Hashtbl.fold
      (fun p () i ->
        count_matches (fun d -> not @@ Hashtbl.mem region Coord.(p + d)) dirs
        + i)
      region 0
  in
  let regions = Hashtbl.create 100 in
  while Hashtbl.length remaining > 0 do
    let ix, iy =
      Hashtbl.to_seq_keys remaining |> Seq.uncons |> Option.get |> fst
    in
    let c = Hashtbl.find table (ix, iy) in
    let region = flood_fill (ix, iy) c in
    Hashtbl.replace regions region ();
    Hashtbl.iter (fun pos () -> Hashtbl.remove remaining pos) region
  done;
  Hashtbl.fold
    (fun region () i -> i + (area region * perimeter region))
    regions 0

let day12b i =
  let lines = In_channel.input_lines i in
  let height = List.length lines in
  let width = List.hd lines |> String.length in
  let table = Hashtbl.create (height * width) in
  let add_row y s =
    List.iteri (fun x c -> Hashtbl.add table (x, y) c) (Util.chars s)
  in
  List.iteri (fun y line -> add_row y line) lines;
  let remaining = Hashtbl.copy table in
  let dirs = [ (-1, 0); (0, -1); (1, 0); (0, 1) ] in
  let flood_fill (ix, iy) chr =
    let region = Hashtbl.create 100 in
    let rec step (x, y) =
      if Hashtbl.mem region (x, y) then ()
      else
        match Hashtbl.find_opt table (x, y) with
        | Some c when c = chr ->
            Hashtbl.add region (x, y) ();
            List.iter (fun (dx, dy) -> step (x + dx, y + dy)) dirs
        | _ -> ()
    in
    step (ix, iy);
    region
  in
  let area region = Hashtbl.length region in
  let perimeter_points region =
    Hashtbl.fold
      (fun (x, y) () i ->
        List.filter_map
          (fun (dx, dy) ->
            if Hashtbl.mem region (x + dx, y + dy) then None
            else Some ((x, y), (dx, dy)))
          dirs
        @ i)
      region []
  in
  let choose table =
    Hashtbl.to_seq_keys table |> Seq.uncons |> Option.get |> fst
  in
  let sides region =
    let pp = Hashtbl.create 100 in
    let counter = ref 0 in
    Hashtbl.replace_seq pp
      (Seq.zip (perimeter_points region |> List.to_seq) (Seq.repeat ()));
    while Hashtbl.length pp > 0 do
      let pos, (dx, dy) = choose pp in
      let rec sweep (ix, iy) (dx, dy) side =
        let d = (ix + dx, iy + dy) in
        if Hashtbl.mem pp (d, side) then (
          Hashtbl.remove pp (d, side);
          sweep d (dx, dy) side)
        else ()
      in
      let sweep_dirs =
        if dx <> 0 then ((0, 1), (0, -1)) else ((1, 0), (-1, 0))
      in
      Hashtbl.remove pp (pos, (dx, dy));
      sweep pos (fst sweep_dirs) (dx, dy);
      sweep pos (snd sweep_dirs) (dx, dy);
      incr counter;
      ()
    done;
    !counter
  in
  let regions = Hashtbl.create 100 in
  while Hashtbl.length remaining > 0 do
    let ix, iy = choose remaining in

    let c = Hashtbl.find table (ix, iy) in
    let region = flood_fill (ix, iy) c in
    Hashtbl.replace regions region ();
    Hashtbl.iter (fun pos () -> Hashtbl.remove remaining pos) region
  done;
  Hashtbl.fold (fun region () i -> i + (area region * sides region)) regions 0

let day13a i =
  let i = Scanf.Scanning.from_channel i in
  let rec scan_all i fmt f =
    match Scanf.bscanf i fmt f with
    | x -> x :: scan_all i fmt f
    | exception End_of_file -> []
  in
  let input =
    scan_all i
      "Button A: X+%d, Y+%d\nButton B: X+%d, Y+%d\nPrize: X=%d, Y=%d\n "
      (fun ax ay bx by tx ty -> ((ax, ay), (bx, by), (tx, ty)))
  in
  let solve a b t =
    let best = ref Int.max_int in
    for ia = 0 to 100 do
      for ib = 0 to 100 do
        let pos = Coord.((a * ia) + (b * ib)) in
        if pos = t then
          let cost = (3 * ia) + ib in
          if cost < !best then best := cost
      done
    done;
    if !best = Int.max_int then None else Some !best
  in
  List.filter_map
    (fun (a, b, t) ->
      Printf.printf "A=%a B=%a T=%a ... %!" Coord.print a Coord.print b
        Coord.print t;
      let solution = solve a b t in
      Printf.printf "-> %s\n%!"
        (Option.fold ~none:"Can't solve" ~some:string_of_int solution);
      solution)
    input
  |> sum

let rec scan_all i fmt f =
  match Scanf.bscanf i fmt f with
  | x -> x :: scan_all i fmt f
  | exception End_of_file -> []

let day13b i =
  let i = Scanf.Scanning.from_channel i in
  let huge = 10_000_000_000_000 in
  let input =
    scan_all i
      "Button A: X+%d, Y+%d\nButton B: X+%d, Y+%d\nPrize: X=%d, Y=%d\n "
      (fun ax ay bx by tx ty -> ((ax, ay), (bx, by), (huge + tx, huge + ty)))
  in
  let solve a b t =
    let best = ref Int.max_int in
    let smaller_x = min (fst a) (fst b) in
    let smaller_y = min (snd a) (snd b) in
    let x_lower_bound = fst t / smaller_x in
    let y_lower_bound = snd t / smaller_y in
    let lower_bound = min x_lower_bound y_lower_bound in
    Printf.printf "sx=%d sy=%d; lx=%d ly=%d; %d %!" smaller_x smaller_y
      x_lower_bound y_lower_bound lower_bound;
    Seq.product (Seq.ints lower_bound) (Seq.ints lower_bound)
    |> Seq.take 1_000_000
    |> Seq.iter (fun (ia, ib) ->
           let pos = Coord.((a * ia) + (b * ib)) in
           if pos = t then
             let cost = (3 * ia) + ib in
             if cost < !best then best := cost);
    if !best = Int.max_int then None else Some !best
  in
  List.filter_map
    (fun (a, b, t) ->
      Printf.printf "A=%a B=%a T=%a ... %!" Coord.print a Coord.print b
        Coord.print t;
      let solution = solve a b t in
      Printf.printf "-> %s\n%!"
        (Option.fold ~none:"Can't solve" ~some:string_of_int solution);
      solution)
    input
  |> sum

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

module Day15 = struct
  type thing = Box | Wall | Air

  let direction = function
    | '^' -> Dir.Up
    | '>' -> Dir.Right
    | 'v' -> Dir.Down
    | '<' -> Dir.Left
    | _ -> failwith "invalid dir"

  let day15a i =
    let map_lines, path_lines = partition_at "" (In_channel.input_lines i) in
    let path = String.concat "" path_lines |> chars |> List.map direction in
    let table = Hashtbl.create 1000 in
    let robot = ref (0, 0) in
    let add_row y s =
      List.iteri
        (fun x c ->
          Hashtbl.replace table (x, y)
            (match c with
            | '#' -> Wall
            | 'O' -> Box
            | '.' -> Air
            | '@' ->
                robot := (x, y);
                Air
            | _ -> failwith "invalid thing"))
        (chars s)
    in
    List.iteri add_row map_lines;
    let move from target =
      Hashtbl.replace table from Air;
      Hashtbl.replace table target Box;
      true
    in
    let rec push from dir =
      let target = Coord.(from >> dir) in
      match Hashtbl.find table target with
      | Air -> move from target
      | Wall -> false
      | Box -> if push target dir then move from target else false
    in
    let simulate move =
      let target = Coord.(!robot >> move) in
      match Hashtbl.find table target with
      | Air -> robot := target
      | Wall -> ()
      | Box -> if push target move then robot := target else ()
    in
    List.iter simulate path;
    Hashtbl.fold
      (fun (x, y) v i -> i + if v = Box then x + (100 * y) else 0)
      table 0

  type wide_thing = BoxLeft | BoxRight | Wall | Air

  let day15b i =
    let map_lines, path_lines = partition_at "" (In_channel.input_lines i) in
    let path = String.concat "" path_lines |> chars |> List.map direction in
    let table = Hashtbl.create 1000 in
    let robot = ref (0, 0) in
    let add_row y s =
      List.iteri
        (fun x c ->
          let left, right =
            match c with
            | '#' -> (Wall, Wall)
            | 'O' -> (BoxLeft, BoxRight)
            | '.' -> (Air, Air)
            | '@' ->
                robot := (x * 2, y);
                (Air, Air)
            | _ -> failwith "invalid thing"
          in
          Hashtbl.replace table (x * 2, y) left;
          Hashtbl.replace table ((x * 2) + 1, y) right)
        (chars s)
    in
    List.iteri add_row map_lines;
    let box_sides from =
      let c = Hashtbl.find table from in
      match c with
      | BoxLeft -> (from, Coord.(from + right))
      | BoxRight -> (Coord.(from + left), from)
      | _ -> failwith "invalid box_sides"
    in
    let move from dir =
      let boxl, boxr = box_sides from in
      Hashtbl.replace table boxl Air;
      Hashtbl.replace table boxr Air;
      Hashtbl.replace table Coord.(boxl >> dir) BoxLeft;
      Hashtbl.replace table Coord.(boxr >> dir) BoxRight
    in
    let rec can_push from dir =
      match dir with
      | Dir.Left | Dir.Right -> check_push Coord.(from >> dir) dir
      | Dir.Down | Dir.Up ->
          let boxl, boxr = box_sides from in
          check_push boxl dir && check_push boxr dir
    and check_push from dir =
      let target = Coord.(from >> dir) in
      match Hashtbl.find table target with
      | Air -> true
      | Wall -> false
      | BoxLeft | BoxRight -> can_push target dir
    in
    let rec do_push from dir =
      match dir with
      | Dir.Left | Dir.Right ->
          chain_push Coord.(from >> dir) dir;
          move from dir
      | Dir.Down | Dir.Up ->
          let boxl, boxr = box_sides from in
          chain_push boxl dir;
          chain_push boxr dir;
          move from dir
    and chain_push from dir =
      let target = Coord.(from >> dir) in
      match Hashtbl.find table target with
      | Air | Wall -> ()
      | BoxLeft | BoxRight -> do_push target dir
    in
    let simulate move =
      let target = Coord.(!robot >> move) in
      match Hashtbl.find table target with
      | Air -> robot := target
      | Wall -> ()
      | BoxLeft | BoxRight ->
          if can_push target move then (
            do_push target move;
            robot := target)
          else ()
    in
    List.iter simulate path;
    Hashtbl.fold
      (fun (x, y) v i -> i + if v = BoxLeft then x + (100 * y) else 0)
      table 0
end

module Day16 = struct
  open Dir

  type xdir = Dir of dir | Omni

  module DirectionalPathfind = Astar.Make (struct
    type t = coord * xdir

    let compare = compare

    let to_string (p, dir) =
      Printf.sprintf "(%s %s)" (Coord.to_string p)
        (match dir with Omni -> "Omni" | Dir d -> Dir.to_string d)
  end)

  let day16a i =
    let lines = In_channel.input_lines i in
    let height = List.length lines in
    let width = List.hd lines |> String.length in
    let table = Hashset.create (height * width) in
    let start = ref (0, 0) in
    let goal = ref (0, 0) in
    let add_row y s =
      List.iteri
        (fun x c ->
          if c = '#' then Hashset.add table (x, y);
          if c = 'S' then start := (x, y);
          if c = 'E' then goal := (x, y))
        (chars s)
    in
    List.iteri (fun y line -> add_row y line) lines;

    let in_bounds (x, y) = x >= 0 && y >= 0 && x < width && y < height in
    let dirs = [ Dir.Up; Dir.Right; Dir.Down; Dir.Left ] in
    let neighbours (p, _) =
      List.filter_map
        (fun ddir ->
          let d = Dir.to_coord ddir in
          let n = Coord.(p + d) in
          if in_bounds n && (not @@ Hashset.mem table n) then
            Some (n, if n = !goal then Omni else Dir ddir)
          else None)
        dirs
    in
    let rotations dir1 dir2 =
      match (dir1, dir2) with
      | _, Omni | Omni, _ -> 0
      | Dir a, Dir b -> (
          match (a, b) with
          | Up, Right
          | Up, Left
          | Right, Up
          | Right, Down
          | Down, Right
          | Down, Left
          | Left, Down
          | Left, Up ->
              1
          | Up, Down | Down, Up | Left, Right | Right, Left -> 2
          | Up, Up | Right, Right | Left, Left | Down, Down -> 0)
    in
    let distance (p1, dir1) (p2, dir2) =
      Coord.manhattan p1 p2 + (rotations dir1 dir2 * 1000)
    in
    let path =
      DirectionalPathfind.pathfind
        (distance (!goal, Omni))
        neighbours distance (!start, Dir Right) (!goal, Omni)
      |> Option.get
    in
    List.fold_left
      (fun (last, cost) next -> (next, cost + distance last next))
      ((!start, Dir Right), 0)
      path
    |> snd

  let day16b i =
    ignore i;
    failwith "TODO"
end

module Day17 = struct
  let scan s =
    Scanf.bscanf s
      "Register A: %d\nRegister B: %d\nRegister C: %d\n\nProgram: %s %!"
      (fun a b c p ->
        ( a,
          b,
          c,
          String.split_on_char ',' p |> List.map int_of_string |> Array.of_list
        ))

  type combo = Literal of int | A | B | C

  type op =
    | Adv of combo
    | Bxl of int
    | Bst of combo
    | Jnz of int
    | Bxc
    | Out of combo
    | Bdv of combo
    | Cdv of combo

  let read_combo x =
    match x with
    | 0 | 1 | 2 | 3 -> Literal x
    | 4 -> A
    | 5 -> B
    | 6 -> C
    | _ -> failwith "invalid combo"

  let string_of_combo = function
    | Literal i -> string_of_int i
    | A -> "A"
    | B -> "B"
    | C -> "C"

  let string_of_op = function
    | Adv c -> Printf.sprintf "adv %s" (string_of_combo c)
    | Bxl i -> Printf.sprintf "bxl %d" i
    | Bst c -> Printf.sprintf "bst %s" (string_of_combo c)
    | Jnz i -> Printf.sprintf "jnz %d" i
    | Bxc -> Printf.sprintf "bxc"
    | Out c -> Printf.sprintf "out %s" (string_of_combo c)
    | Bdv c -> Printf.sprintf "bdv %s" (string_of_combo c)
    | Cdv c -> Printf.sprintf "cdv %s" (string_of_combo c)

  type state = {
    a : int;
    b : int;
    c : int;
    ip : int;
    ops : int array;
    out : int list;
  }

  let next state = { state with ip = state.ip + 2 }

  let resolve_combo state = function
    | Literal x -> x
    | A -> state.a
    | B -> state.b
    | C -> state.c

  let dv state c =
    Printf.printf "dv %d / %d (2^%s = 2^%d) = %d\n%!" state.a
      (pow2 (resolve_combo state c))
      (string_of_combo c) (resolve_combo state c)
      (state.a / pow2 (resolve_combo state c));
    state.a / pow2 (resolve_combo state c)

  let read_op a b =
    match a with
    | 0 -> Adv (read_combo b)
    | 1 -> Bxl b
    | 2 -> Bst (read_combo b)
    | 3 -> Jnz b
    | 4 -> Bxc
    | 5 -> Out (read_combo b)
    | 6 -> Bdv (read_combo b)
    | 7 -> Cdv (read_combo b)
    | _ -> failwith "nope"

  let eval state = function
    | Adv c -> next { state with a = dv state c }
    | Bdv c -> next { state with b = dv state c }
    | Cdv c -> next { state with c = dv state c }
    | Bxl i -> next { state with b = state.b lxor i }
    | Bst c -> next { state with b = resolve_combo state c land 0b111 }
    | Jnz i -> { state with ip = (if state.a = 0 then state.ip + 2 else i) }
    | Bxc -> next { state with b = state.b lxor state.c }
    | Out c ->
        next
          {
            state with
            out = (resolve_combo state c |> ( land ) 0b111) :: state.out;
          }

  let eval_all a b c ops =
    let state = { a; b; c; ops; ip = 0; out = [] } in
    let rec step state =
      Printf.printf "a:%d b:%d c:%d ip:%d\n%!" state.a state.b state.c state.ip;
      if state.ip >= Array.length state.ops then state
      else
        let op = read_op state.ops.(state.ip) state.ops.(state.ip + 1) in
        Printf.printf "[%d] %s\n%!" state.ip (string_of_op op);
        step (eval state op)
    in
    (step state).out |> List.rev |> List.map string_of_int |> String.concat ","

  let day17a i =
    let i = Scanf.Scanning.from_channel i in
    let a, b, c, ops = scan i in
    eval_all a b c ops |> print_endline;
    0

  let day17b i =
    ignore i;
    failwith "TODO"
end

let day18a i =
  let input = scan_lines i "%d,%d" (fun a b -> (a, b)) in
  let size, bytes = (71, 1024) in
  (* let size, bytes = 7, 12 in *)
  let start = (0, 0) in
  let goal = (size - 1, size - 1) in
  let table = Hashset.create (size * size) in
  List.to_seq input |> Seq.take bytes |> Seq.iter (Hashset.add table);
  let path = cardinal_pathfind ~width:size ~height:size table start goal in
  match path with
  | None -> failwith "No path found"
  | Some path -> List.length path - 1

let day18b i =
  let input = scan_lines i "%d,%d" (fun a b -> (a, b)) in
  let size = 71 in
  (* let size = 7 in *)
  let start = (0, 0) in
  let goal = (size - 1, size - 1) in
  let fall n =
    let table = Hashset.create (size * size) in
    List.to_seq input |> Seq.take n |> Seq.iter (Hashset.add table);
    cardinal_pathfind ~width:size ~height:size table start goal
  in
  let out (x, y) = Printf.sprintf "%d,%d" x y in
  binary_searchi 1024 (List.length input) (fun n ->
      fall (n + 1) |> Option.is_none)
  |> List.nth input |> out |> print_endline;
  0

let parse_day19 i =
  let available =
    In_channel.input_line i |> Option.get |> String.split_on_char ','
    |> List.map String.trim
  in
  let target = In_channel.input_lines i |> List.filter (( <> ) "") in
  (available, target)

let day19a i =
  let available, target = parse_day19 i in
  let rec possible available target =
    available
    |> List.exists (fun prefix ->
           if String.starts_with ~prefix target then
             let plen = String.length prefix in
             let tlen = String.length target in
             if plen = tlen then true
             else possible available (String.sub target plen (tlen - plen))
           else false)
  in
  count_matches (possible available) target

let day19b i =
  let available, target = parse_day19 i in
  let cache = Hashtbl.create 1_000_000 in
  let rec count_possible target =
    let aux () =
      available
      |> List.map (fun prefix ->
             if String.starts_with ~prefix target then
               let plen = String.length prefix in
               let tlen = String.length target in
               if plen = tlen then 1
               else count_possible (String.sub target plen (tlen - plen))
             else 0)
      |> sum
    in
    match Hashtbl.find_opt cache target with
    | Some x -> x
    | None ->
        let r = aux () in
        Hashtbl.replace cache target r;
        r
  in
  List.map count_possible target |> sum

let parse_day20 i =
  let lines = In_channel.input_lines i in
  let height = List.length lines in
  let width = List.hd lines |> String.length in
  let table = Hashset.create (height * width) in
  let start = ref (0, 0) in
  let goal = ref (0, 0) in
  let add_row y s =
    List.iteri
      (fun x c ->
        if c = '#' then Hashset.add table (x, y);
        if c = 'S' then start := (x, y);
        if c = 'E' then goal := (x, y))
      (chars s)
  in
  List.iteri (fun y line -> add_row y line) lines;
  cardinal_pathfind ~width ~height table !start !goal |> Option.get

let day20a i =
  let path = parse_day20 i in
  let cheat from rest =
    Coord.[ up * 2; right * 2; down * 2; left * 2 ]
    |> List.map (Coord.( + ) from)
    |> List.filter_map (fun s -> List.find_index (( = ) s) rest)
    |> List.filter (( < ) 100)
    |> List.length
  in
  let rec cheats = function [] -> 0 | x :: xs -> cheat x xs + cheats xs in
  cheats path

let day20b i =
  let path = parse_day20 i in
  let cheat from rest =
    rest
    |> List.mapi (fun i x -> (Coord.manhattan from x, i + 1))
    |> List.filter (fun (m, _) -> m <= 20)
    |> List.filter (fun (m, d) -> m <> d)
    |> List.map (fun (m, d) -> d - m)
    |> List.filter (( <= ) 100)
    |> List.length
  in
  let rec cheats = function [] -> 0 | x :: xs -> cheat x xs + cheats xs in
  cheats path

module Day21 = struct
  let find_in_keypad (kp : char list list) needle =
    List.find_mapi
      (fun y ->
        List.find_mapi (fun x c -> if c = needle then Some (x, y) else None))
      kp
    |> Option.get

  let num_kp =
    [
      [ '7'; '8'; '9' ]; [ '4'; '5'; '6' ]; [ '1'; '2'; '3' ]; [ ' '; '0'; 'A' ];
    ]

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
end

module Day22 = struct
  let mix = ( lxor )
  let prune a = a mod 16777216

  let step secret =
    let secret = secret * 64 |> mix secret |> prune in
    let secret =
      float_of_int secret /. 32.0
      |> floor |> int_of_float |> mix secret |> prune
    in
    let secret = secret * 2048 |> mix secret |> prune in
    secret

  let day22a i =
    let simulate initial =
      Seq.iterate step initial |> Seq.drop 2000 |> Util.head
    in
    In_channel.input_lines i |> List.map int_of_string |> List.map simulate
    |> sum

  let day22b i =
    let simulate initial =
      Seq.iterate step initial
      |> Seq.map (fun i -> i mod 10)
      |> Seq.scan (fun (prev, _) price -> (price, price - prev)) (0, 0)
      |> Seq.drop 2 |> Seq.take 2000
      |> Seq.scan
           (fun ((_, d2, d3, d4), _) (price, diff) ->
             ((d2, d3, d4, diff), price))
           ((0, 0, 0, 0), 0)
      |> Seq.drop 4
    in
    let results seq =
      let table = Hashtbl.create 2000 in
      Seq.iter
        (fun (k, v) ->
          match Hashtbl.find_opt table k with
          | Some _ -> ()
          | None -> Hashtbl.add table k v)
        seq;
      table
    in
    let merge m h1 h2 =
      Hashtbl.iter
        (fun k v ->
          let n =
            match Hashtbl.find_opt h1 k with
            | Some existing -> m existing v
            | None -> v
          in
          Hashtbl.replace h1 k n)
        h2
    in
    In_channel.input_lines i |> List.map int_of_string
    |> List.fold_left
         (fun h i ->
           simulate i |> results |> merge ( + ) h;
           h)
         (Hashtbl.create 1_000_000)
    |> Hashtbl.to_seq_values |> Seq.fold_left max 0
end

let parse_day23 i =
  let nodes =
    In_channel.input_lines i
    |> List.map (String.split_on_char '-')
    |> List.map as_pair
  in
  let graph = Hashtbl.create 1000 in
  List.iter
    (fun (a, b) ->
      Hashtbl.add graph a b;
      Hashtbl.add graph b a)
    nodes;
  (nodes, graph)

let day23a i =
  let nodes, graph = parse_day23 i in
  let triplets = Hashset.create 1000 in
  List.iter
    (fun (k, v) ->
      let kset = Hashtbl.find_all graph k |> StringSet.of_list in
      let vset = Hashtbl.find_all graph v |> StringSet.of_list in
      let inter = StringSet.inter kset vset in
      StringSet.iter
        (fun c -> Hashset.add triplets (List.sort String.compare [ k; v; c ]))
        inter)
    nodes;
  Hashset.to_seq triplets
  |> Seq.filter (fun triplet -> triplet |> List.exists (fun s -> s.[0] = 't'))
  |> Seq.length

let day23b i =
  let nodes, graph = parse_day23 i in
  let intersets = Hashset.create 1_000_000 in
  let rec common_intersections points =
    let key = List.sort String.compare points |> String.concat "," in
    if Hashset.mem intersets key then ()
    else (
      Hashset.add intersets key;
      let sets =
        List.map (fun p -> Hashtbl.find_all graph p |> StringSet.of_list) points
      in
      List.fold_left (fun acc s -> StringSet.inter acc s) (List.hd sets) sets
      |> StringSet.iter (fun common -> common_intersections (common :: points)))
  in
  List.iter (fun (k, v) -> common_intersections [ k; v ]) nodes;
  intersets |> Hashset.to_seq
  |> Seq.fold_left
       (fun best i -> if String.length i > String.length best then i else best)
       ""
  |> print_endline;
  0

let () =
  let problem = Sys.argv.(1) in
  let variant = Sys.argv.(2) in
  let solver : solver =
    match (problem, variant) with
    | "2020day1", "a" -> Aoc2020.day1a
    | "2020day1", "b" -> Aoc2020.day1b
    | "2020day2", "a" -> Aoc2020.day2a
    | "2020day2", "b" -> Aoc2020.day2b
    | "2020day3", "a" -> Aoc2020.day3a
    | "2020day3", "b" -> Aoc2020.day3b
    | "2020day4", "a" -> Aoc2020.day4a
    | "2020day4", "b" -> Aoc2020.day4b
    | "2020day13", "a" -> Aoc2020.day13a
    | "2020day13", "b" -> failwith "todo"
    | "2020day15", "a" -> Aoc2020.Day15.day15a
    | "2020day15", "b" -> Aoc2020.Day15.day15b
    | "day1", "a" -> day1a
    | "day1", "b" -> day1b
    | "day2", "a" -> day2a
    | "day2", "b" -> day2b
    | "day3", "a" -> day3a
    | "day3", "b" -> day3b
    | "day4", "a" -> day4a
    | "day4", "b" -> day4b
    | "day5", "a" -> Day5.day5a
    | "day5", "b" -> Day5.day5b
    | "day6", "a" -> Day6.day6a
    | "day6", "b" -> Day6.day6b
    | "day7", "a" -> day7a
    | "day7", "b" -> day7b
    | "day8", "a" -> day8a
    | "day8", "b" -> day8b
    | "day9", "a" -> day9a
    | "day9", "b" -> day9b
    | "day10", "a" -> day10a
    | "day10", "b" -> day10b
    | "day11", "a" -> day11a
    | "day11", "b" -> day11b
    | "day12", "a" -> day12a
    | "day12", "b" -> day12b
    | "day13", "a" -> day13a
    | "day13", "b" -> day13b
    | "day14", "a" -> day14a
    | "day14", "b" -> day14b
    | "day15", "a" -> Day15.day15a
    | "day15", "b" -> Day15.day15b
    | "day16", "a" -> Day16.day16a
    | "day16", "b" -> Day16.day16b
    | "day17", "a" -> Day17.day17a
    | "day17", "b" -> Day17.day17b
    | "day18", "a" -> day18a
    | "day18", "b" -> day18b
    | "day19", "a" -> day19a
    | "day19", "b" -> day19b
    | "day20", "a" -> day20a
    | "day20", "b" -> day20b
    | "day21", "a" -> Day21.day21a
    | "day21", "b" -> Day21.day21b
    | "day22", "a" -> Day22.day22a
    | "day22", "b" -> Day22.day22b
    | "day23", "a" -> day23a
    | "day23", "b" -> day23b
    | _ -> failwith (Printf.sprintf "Unknown problem %s %s" problem variant)
  in
  solve solver problem
