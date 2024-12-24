open Aoclib
open Aoclib.Util

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
  let facing = ref Dir.Up in
  let visited = ref @@ CoordSet.empty in
  let rec move () =
    if BinaryMap2D.oob !current map then ()
    else (
      visited := CoordSet.add !current !visited;
      let next = Coord.(!current >> !facing) in
      if BinaryMap2D.has next map then facing := Dir.rotate_cw !facing
      else current := next;
      move ())
  in
  move ();
  CoordSet.to_seq !visited |> Seq.length

let day6b i =
  let start, map = parse_day6 i in
  let visited = Hashset.create 2000 in
  let loops_with_obstacle obs =
    let current = ref start in
    let facing = ref Dir.Up in
    let map =
      Option.fold ~none:map ~some:(fun o -> BinaryMap2D.add o map) obs
    in
    let visited_facing = Hashset.create 2000 in
    Hashset.clear visited;
    let rec loops () =
      if BinaryMap2D.oob !current map then false
      else if Hashset.mem visited_facing (!current, !facing) then true
      else (
        Hashset.add visited !current;
        Hashset.add visited_facing (!current, !facing);
        let next = Coord.(!current >> !facing) in
        if BinaryMap2D.has next map then facing := Dir.rotate_cw !facing
        else current := next;
        loops ())
    in
    loops ()
  in
  loops_with_obstacle None |> ignore;
  Hashset.remove visited start;
  let path = Hashset.to_seq visited |> Array.of_seq in
  let pool = Domainslib.Task.setup_pool ~num_domains:6 () in
  Domainslib.Task.run pool (fun () ->
      Domainslib.Task.parallel_for_reduce ~start:0
        ~finish:(Array.length path - 1)
        ~body:(fun i -> if loops_with_obstacle (Some path.(i)) then 1 else 0)
        pool ( + ) 0)
