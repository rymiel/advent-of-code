open Aoclib
open Aoclib.Util

type scan = { sensor : coord; beacon : coord; radius : int }

let parse i =
  scan_lines i "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
    (fun sx sy bx by ->
      {
        sensor = (sx, sy);
        beacon = (bx, by);
        radius = Coord.manhattan (sx, sy) (bx, by);
      })

let day15a i =
  let input = parse i in
  let extremes =
    List.to_seq input
    |> Seq.flat_map (fun scan ->
           let sensor_x = fst scan.sensor in
           List.to_seq
             [ sensor_x - scan.radius - 1; sensor_x + scan.radius + 1 ])
  in
  let count = ref 0 in
  let y = 2000000 in
  for x = SeqExt.min extremes to SeqExt.max extremes do
    let pos = (x, y) in
    if
      List.exists
        (fun scan ->
          scan.beacon <> pos && Coord.manhattan scan.sensor pos <= scan.radius)
        input
    then incr count
  done;
  !count

let day15b i =
  let input = parse i in
  let perimeter_points scan =
    let lines =
      [
        ((0, -1), (1, 1));
        ((1, 0), (-1, 1));
        ((0, 1), (-1, -1));
        ((-1, 0), (1, -1));
      ]
    in
    List.to_seq lines
    |> Seq.flat_map (fun (start_offset, slope) ->
           let outline_radius = scan.radius + 1 in
           let start = Coord.(scan.sensor + (start_offset * outline_radius)) in
           Seq.iterate (Coord.( + ) slope) start |> Seq.take outline_radius)
  in
  let not_scanned pos =
    List.for_all
      (fun scan -> Coord.manhattan scan.sensor pos > scan.radius)
      input
  in
  let limit = 4000000 in
  List.to_seq input
  |> Seq.flat_map perimeter_points
  |> Seq.filter (fun (x, y) -> x >= 0 && y >= 0 && x <= limit && y <= limit)
  |> Seq.find not_scanned |> Option.get
  |> fun (x, y) -> (x * 4000000) + y
