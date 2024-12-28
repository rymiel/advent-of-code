open Aoclib.Util

type move = { count : int; from : int; target : int }

let parse i =
  let crates, moves = In_channel.input_lines i |> partition_at "" in
  let moves =
    List.map
      (fun s ->
        Scanf.sscanf s "move %d from %d to %d%!" (fun count from target ->
            { count; from; target }))
      moves
  in
  let crates = List.rev crates |> List.tl in
  let max_width = crates |> List.map String.length |> ListExt.max in
  let crate_width = (max_width + 1) / 4 in
  let indices =
    Seq.ints 0 |> Seq.take crate_width
    |> Seq.map (fun i -> (i * 4) + 1)
    |> List.of_seq
  in
  let stacks =
    crates
    |> List.map (fun s -> indices |> List.map (fun i -> s.[i]) |> List.to_seq)
    |> List.to_seq |> Seq.transpose
    |> Seq.map (fun cs -> cs |> Seq.filter (( <> ) ' ') |> Stack.of_seq)
    |> Array.of_seq
  in
  (stacks, moves)

let day5a i =
  let stacks, moves = parse i in
  let move_single from target =
    let hold = Stack.pop stacks.(from - 1) in
    Stack.push hold stacks.(target - 1)
  in
  let move { count; from; target } =
    Seq.repeat () |> Seq.take count
    |> Seq.iter (fun () -> move_single from target)
  in
  List.iter move moves;
  Array.map (fun stack -> Stack.top stack |> string_of_char) stacks
  |> Array.to_list |> String.concat "" |> print_endline;
  0

let day5b i =
  let stacks, moves = parse i in
  let take from = Stack.pop stacks.(from - 1) in
  let put target list = Stack.add_seq stacks.(target - 1) (List.to_seq list) in
  let move { count; from; target } =
    Seq.repeat () |> Seq.take count
    |> Seq.map (fun () -> take from)
    |> List.of_seq |> List.rev |> put target
  in
  List.iter move moves;
  Array.map (fun stack -> Stack.top stack |> string_of_char) stacks
  |> Array.to_list |> String.concat "" |> print_endline;
  0
