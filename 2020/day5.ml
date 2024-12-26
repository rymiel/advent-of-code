open Aoclib.Util

let parse_day5 i =
  In_channel.input_lines i
  |> List.map (fun line ->
         let dispense = line |> String.to_seq |> Seq.to_dispenser in
         let row =
           binary_searchi 0 127 (fun _ -> Option.get (dispense ()) = 'F')
         in
         let column =
           binary_searchi 0 7 (fun _ -> Option.get (dispense ()) = 'L')
         in
         (row * 8) + column)

let day5a i = parse_day5 i |> List.fold_left max 0

let day5b i =
  parse_day5 i |> List.sort compare |> List.to_seq
  |> Seq.scan (fun (prev, _diff) curr -> (curr, curr - prev)) (0, 0)
  |> Seq.drop 2
  |> Seq.find_map (fun (num, diff) -> if diff = 2 then Some (num - 1) else None)
  |> Option.get
