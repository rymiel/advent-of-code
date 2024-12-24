open Aoclib.Util

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
