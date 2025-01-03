open Aoclib.Util

type valve = { flow : int; neighbours : string list }

module GraphPathfind = Aoclib.Astar.Make (String)

type node = { h : int; s : int; sec : int; cur : string; seen : StringSet.t }

module PQSet = Set.Make (struct
  type t = node

  let compare = compare
end)

let parse i =
  let valves = Hashtbl.create 1000 in
  let input =
    scan_lines i "Valve %s has flow rate=%d; %[^A-Z] %[^\n]"
      (fun id flow _ neighbours ->
        let neighbours =
          String.split_on_char ',' neighbours |> List.map String.trim
        in
        (id, { flow; neighbours }))
  in
  Hashtbl.add_seq valves (List.to_seq input);
  valves

let build_data valves =
  let useful_valves =
    Hashtbl.to_seq valves
    |> Seq.filter (fun (_, valve) -> valve.flow > 0)
    |> Seq.map fst |> Seq.memoize
  in
  let useful = StringSet.of_seq useful_valves in

  (* cba *)
  let gp_heuristic = Fun.const 0 in
  let gp_neighbours n = (Hashtbl.find valves n).neighbours in
  let gp_distance _ _ = 1 in
  let distances = Hashtbl.create 1_000_000 in
  Seq.product (Seq.append (Seq.return "AA") useful_valves) useful_valves
  |> Seq.iter (fun (a, b) ->
         let d =
           GraphPathfind.pathfind gp_heuristic gp_neighbours gp_distance a b
           |> Option.get |> List.length
         in
         Hashtbl.add distances (a, b) (d - 1));
  (useful, distances)

let day16a i =
  let valves = parse i in
  let useful, distances = build_data valves in
  let initial =
    { h = 0; s = 0; sec = 30; cur = "AA"; seen = StringSet.empty }
  in
  let frontier = ref PQSet.(empty |> add initial) in
  let record = ref 0 in
  let get_best () =
    let min = PQSet.max_elt !frontier in
    frontier := PQSet.remove min !frontier;
    min
  in
  let rec loop () =
    if not @@ PQSet.is_empty !frontier then (
      let best = get_best () in
      let rest = StringSet.diff useful best.seen in
      let flow_rate =
        StringSet.fold (fun s i -> i + (Hashtbl.find valves s).flow) best.seen 0
      in
      let total = best.s + (flow_rate * best.sec) in
      let ideal =
        StringSet.fold
          (fun s i -> i + ((Hashtbl.find valves s).flow * best.sec))
          rest total
      in

      if ideal >= !record then (
        if total > !record then record := total;

        StringSet.iter
          (fun n ->
            let d = Hashtbl.find distances (best.cur, n) in
            let n_valve = Hashtbl.find valves n in
            let lapse = d + 1 in
            let remaining = best.sec - lapse in
            let opportunity = remaining * n_valve.flow in
            if remaining >= 0 then
              frontier :=
                PQSet.add
                  {
                    h = best.h + opportunity;
                    s = best.s + (lapse * flow_rate);
                    sec = remaining;
                    cur = n;
                    seen = StringSet.add n best.seen;
                  }
                  !frontier)
          rest);
      loop ())
  in
  loop ();
  !record

(* type node2 = {
     h : int;
     s : int;
     sec : int;
     cur1 : string;
     cur2 : string;
     seen : StringSet.t;
     path1 : string list;
     path2 : string list;
   }

   module PQSet2 = Set.Make (struct
     type t = node2

     let compare = compare
   end) *)

let day16b i =
  (* let valves = parse i in
     let useful, distances = build_data valves in
     let initial =
       {
         h = 0;
         s = 0;
         sec = 30;
         cur1 = "AA";
         cur2 = "AA";
         seen = StringSet.empty;
         path1 = [ "AA" ];
         path2 = [ "AA" ];
       }
     in
     let frontier = ref PQSet2.(empty |> add initial) in
     let record = ref 0 in
     let path = ref ([], []) in
     let get_best () =
       let min = PQSet2.max_elt !frontier in
       frontier := PQSet2.remove min !frontier;
       min
     in
     let rec loop () =
       if PQSet2.is_empty !frontier then ()
       else
         let best = get_best () in
         let rest = StringSet.diff useful best.seen in
         let flow_rate =
           StringSet.fold (fun s i -> i + (Hashtbl.find valves s).flow) best.seen 0
         in
         let total = best.s + (flow_rate * best.sec) in
         let ideal =
           StringSet.fold
             (fun s i -> i + ((Hashtbl.find valves s).flow * best.sec))
             rest total
         in
         Printf.printf
           "Frontier: h:%d s:%d at (%s,%s), %d ideal, %d left, %d record, (%s,%s)\n"
           best.h best.s best.cur1 best.cur2 ideal best.sec !record
           (String.concat "<--" best.path1)
           (String.concat "<--" best.path2);

         if ideal >= !record then (
           if StringSet.is_empty rest then
             Printf.printf "*** *** *** Ran out of targets. End result: %d\n" total;
           if total > !record then (
             record := total;
             path := (best.path1, best.path2));

           StringSet.iter
             (fun n ->
               let d = Hashtbl.find distances (best.cur, n) in
               let n_valve = Hashtbl.find valves n in
               let lapse = d + 1 in
               let remaining = best.sec - lapse in
               let opportunity = remaining * n_valve.flow in
               Printf.printf
                 "%s -> %s: d:%d opportunity:%d (%d * %d) sec:%d-%d=%d\n" best.cur
                 n d opportunity remaining n_valve.flow best.sec lapse remaining;
               if remaining >= 0 then
                 frontier :=
                   PQSet.add
                     {
                       h = best.h + opportunity;
                       s = best.s + (lapse * flow_rate);
                       sec = remaining;
                       cur = n;
                       seen = StringSet.add n best.seen;
                       path = n :: best.path;
                     }
                     !frontier
               else
                 (* let total = best.s + (flow_rate * best.sec) in
                    Printf.printf "*** *** *** Ran out of time. End result: %d\n" total; *)
                 ())
             rest)
         else
           (* Printf.printf "Pruned: %s at %d: %d ideal vs %d record\n"
              (String.concat "<--" best.path)
              best.sec ideal !record; *)
           ();
         loop ()
     in
     loop ();
     Printf.printf "Record: %d\n" !record;
     Printf.printf "Path: %s\n" (String.concat "<--" !path);
     0 *)
  ignore i;
  failwith "TODO"
