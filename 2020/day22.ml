open Aoclib
open Aoclib.Util

let parse i =
  In_channel.input_lines i |> partition_at ""
  |> Pair.map (fun l -> List.tl l |> List.map int_of_string)

let day22a i =
  let p1, p2 = parse i in
  let do_round = function
    | p1_head :: p1_rest, p2_head :: p2_rest ->
        if p1_head > p2_head then (p1_rest @ [ p1_head; p2_head ], p2_rest)
        else (p1_rest, p2_rest @ [ p2_head; p1_head ])
    | _, _ -> failwith "invalid"
  in
  let score l =
    List.rev l |> List.mapi (fun i n -> (i + 1) * n) |> ListExt.sum
  in
  Seq.iterate do_round (p1, p2)
  |> Seq.drop_while (fun (p1, p2) ->
         not @@ (List.is_empty p1 || List.is_empty p2))
  |> head |> Pair.map score |> Pair.apply ( + )

let day22b i =
  let p1, p2 = parse i in
  let score l =
    List.rev l |> List.mapi (fun i n -> (i + 1) * n) |> ListExt.sum
  in
  let rec try_round cache = function
    | (p1_head :: p1_rest as p1), (p2_head :: p2_rest as p2) ->
        if Hashset.mem cache (p1, p2) then
          (* immediate game win for player 1 *)
          (p1 @ p2, [])
        else (
          Hashset.add cache (p1, p2);
          do_round p1_head p1_rest p2_head p2_rest)
    | _, _ -> failwith "invalid"
  and do_round p1_head p1_rest p2_head p2_rest =
    let p1_wins =
      if List.length p1_rest >= p1_head && List.length p2_rest >= p2_head then
        (* recurse *)
        do_game
          ( List.to_seq p1_rest |> Seq.take p1_head |> List.of_seq,
            List.to_seq p2_rest |> Seq.take p2_head |> List.of_seq )
        |> Either.is_left
      else (* normal rules *) p1_head > p2_head
    in
    if p1_wins then (p1_rest @ [ p1_head; p2_head ], p2_rest)
    else (p1_rest, p2_rest @ [ p2_head; p1_head ])
  and do_game (p1, p2) =
    let cache = Hashset.create 100 in
    Seq.iterate (try_round cache) (p1, p2)
    |> Seq.drop_while (fun (p1, p2) ->
           not @@ (List.is_empty p1 || List.is_empty p2))
    |> head
    |> function
    | p1, [] -> Either.left (score p1)
    | [], p2 -> Either.right (score p2)
    | _, _ -> failwith "invalid"
  in
  do_game (p1, p2) |> Either.fold ~left:Fun.id ~right:Fun.id
