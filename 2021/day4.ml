open Aoclib.Util

let rows_and_columns lines =
  let size = List.length lines in
  lines
  @ (List.init size Fun.id
    |> List.map (fun i -> List.map (fun s -> List.nth s i) lines))

let board_has_bingo nums board =
  List.exists
    (fun row -> List.for_all (fun num -> List.mem num nums) row)
    (rows_and_columns board)

let score (nums, board) =
  let unmarked =
    List.filter (fun i -> not @@ List.mem i nums) (List.concat board)
    |> ListExt.sum
  in
  unmarked * List.hd nums

let parse i =
  let numbers, boards =
    In_channel.input_lines i |> partition_all "" |> ListExt.uncons |> Option.get
  in
  let numbers =
    List.hd numbers |> String.split_on_char ',' |> List.map int_of_string
  in
  let boards =
    List.map (List.map (fun s -> scan_all_str s " %d " Fun.id)) boards
  in
  (numbers, boards)

let day4a i =
  let numbers, boards = parse i in
  List.to_seq numbers
  |> Seq.scan (fun l i -> i :: l) []
  |> Seq.find_map (fun nums ->
         List.find_opt (board_has_bingo nums) boards
         |> Option.map (fun b -> (nums, b)))
  |> Option.get |> score

let day4b i =
  let numbers, boards = parse i in
  List.to_seq numbers
  |> Seq.scan (fun l i -> i :: l) []
  |> Seq.map (fun nums -> (nums, List.filter (board_has_bingo nums) boards))
  |> sliding_pair
  |> Seq.map (fun ((_, prev_board), (nums, cur_board)) ->
         (nums, List.find_opt (fun b -> not @@ List.mem b prev_board) cur_board))
  |> Seq.filter_map (fun (nums, board) -> Option.map (fun b -> (nums, b)) board)
  |> SeqExt.last |> score
