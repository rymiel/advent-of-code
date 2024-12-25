module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type node
  type weight = int
  type heuristic = node -> weight
  type neighbours = node -> node list
  type distance = node -> node -> weight

  val pathfind :
    heuristic -> neighbours -> distance -> node -> node -> node list option
end

module Make (Node : OrderedType) : S with type node := Node.t = struct
  type node = Node.t
  type weight = int
  type heuristic = node -> weight
  type neighbours = node -> node list
  type distance = node -> node -> weight

  let reconstruct_path came_from current =
    let rec aux acc current =
      match Hashtbl.find_opt came_from current with
      | None -> acc
      | Some from -> aux (from :: acc) from
    in
    aux [ current ] current

  module PQSet = Set.Make (struct
    type t = int * node

    let compare = compare
  end)

  let pathfind (h : heuristic) (n : neighbours) (d : distance) (start : node)
      (goal : node) =
    let came_from = Hashtbl.create 100 in
    let cost_so_far = Hashtbl.create 100 in
    Hashtbl.add cost_so_far start 0;
    let frontier = ref PQSet.(empty |> add (h start, start)) in
    let get_best () =
      let min = PQSet.min_elt !frontier in
      frontier := PQSet.remove min !frontier;
      snd min
    in
    (* Printf.printf "problem: %s -> %s\n%!" (Node.to_string start)
       (Node.to_string goal); *)
    let rec loop () =
      if PQSet.is_empty !frontier then None
      else
        let current = get_best () in
        if current = goal then Some (reconstruct_path came_from current)
        else (
          (* Printf.printf "%s -> [%s]\n%!" (Node.to_string current)
             (n current |> List.map Node.to_string |> String.concat "; "); *)
          List.iter
            (fun next ->
              let new_cost =
                Hashtbl.find cost_so_far current + d current next
              in
              if
                match Hashtbl.find_opt cost_so_far next with
                | None -> true
                | Some g_next -> new_cost < g_next
              then (
                Hashtbl.replace came_from next current;
                Hashtbl.replace cost_so_far next new_cost;
                let priority = new_cost + h next in
                frontier := PQSet.add (priority, next) !frontier))
            (n current);
          loop ())
    in
    loop ()
end
