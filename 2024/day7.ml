open Aoclib.Util

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
