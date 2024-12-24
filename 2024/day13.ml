open Aoclib
open Aoclib.Util

let day13a i =
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
  List.filter_map (fun (a, b, t) -> solve a b t) input |> sum

let day13b i =
  let huge = 10_000_000_000_000 in
  let input =
    scan_all i
      "Button A: X+%d, Y+%d\nButton B: X+%d, Y+%d\nPrize: X=%d, Y=%d\n "
      (fun ax ay bx by tx ty -> ((ax, ay), (bx, by), (huge + tx, huge + ty)))
  in
  let solve (ax, ay) (bx, by) (tx, ty) =
    (* bad problem *)
    let denom = (ax * by) - (ay * bx) in
    let a = ((tx * by) - (bx * ty)) / denom in
    let b = ((ax * ty) - (tx * ay)) / denom in
    if Coord.(((ax, ay) * a) + ((bx, by) * b)) = (tx, ty) then Some ((a * 3) + b)
    else None
  in
  List.filter_map (fun (a, b, t) -> solve a b t) input |> sum
