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

module Make : functor (Node : OrderedType) -> S with type node := Node.t
