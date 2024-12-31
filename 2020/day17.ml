open Aoclib
open Aoclib.Util

module type CoordSystem = sig
  type t

  val ( + ) : t -> t -> t
  val directions : t list
  val flat : int * int -> t
end

let add_dimension with_axis zero_previous previous =
  List.concat
    [
      List.map (with_axis (-1)) previous;
      List.map (with_axis 0) previous;
      List.map (with_axis 1) previous;
      [ with_axis (-1) zero_previous; with_axis 1 zero_previous ];
    ]

let direction1 = [ 1; -1 ]
let directions2 = add_dimension (fun y x -> (x, y)) 0 direction1
let directions3 = add_dimension (fun z (x, y) -> (x, y, z)) (0, 0) directions2

let directions4 =
  add_dimension (fun w (x, y, z) -> (x, y, z, w)) (0, 0, 0) directions3

module Solution (CS : CoordSystem) = struct
  let adjacent_active table pos =
    ListExt.count_matches
      (fun d -> Hashset.mem table CS.(pos + d))
      CS.directions

  let step table =
    let new_table = Hashset.copy table in
    Hashset.filter_inplace
      (fun pos ->
        let on_adj = adjacent_active table pos in
        if on_adj = 2 || on_adj = 3 then true else false)
      new_table;
    let frontier = Hashset.create (Hashset.length table) in
    Hashset.iter
      (fun pos ->
        List.iter
          (fun d ->
            let n = CS.(pos + d) in
            if not @@ Hashset.mem table n then Hashset.add frontier n)
          CS.directions)
      table;
    Hashset.iter
      (fun pos ->
        let on_adj = adjacent_active table pos in
        if on_adj = 3 then Hashset.add new_table pos)
      frontier;
    new_table

  let parse i =
    let set = Hashset.create 1_000_000 in
    read_matrix i (fun (x, y) c ->
        if c = '#' then Hashset.add set (CS.flat (x, y)));
    set
end

module Part1 = Solution (struct
  type t = Coord3.coord3

  let ( + ) = Coord3.( + )
  let directions = directions3
  let flat (x, y) = (x, y, 0)
end)

let solve parse step i =
  Seq.iterate step (parse i) |> Seq.map Hashset.length |> Seq.drop 6 |> head

let day17a i = solve Part1.parse Part1.step i

module Part2 = Solution (struct
  type t = int * int * int * int

  let ( + ) (ax, ay, az, aw) (bx, by, bz, bw) =
    (ax + bx, ay + by, az + bz, aw + bw)

  let directions = directions4
  let flat (x, y) = (x, y, 0, 0)
end)

let day17b i = solve Part2.parse Part2.step i
