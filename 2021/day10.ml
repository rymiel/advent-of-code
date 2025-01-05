open Aoclib.Util

let illegal_score = function
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | _ -> failwith "invalid"

let completion_char_score = function
  | ')' -> 1
  | ']' -> 2
  | '}' -> 3
  | '>' -> 4
  | _ -> failwith "invalid"

let rec completion_score i = function
  | [] -> i
  | x :: xs -> completion_score ((i * 5) + completion_char_score x) xs

let rec syntax_error_score stack line =
  match (line, stack) with
  | "", [] -> Either.left 0
  | "", _ -> Either.right (completion_score 0 stack)
  | _, _ -> (
      let line_head = line.[0] in
      let line_tail = String.sub line 1 (String.length line - 1) in
      match line_head with
      | '(' -> syntax_error_score (')' :: stack) line_tail
      | '[' -> syntax_error_score (']' :: stack) line_tail
      | '{' -> syntax_error_score ('}' :: stack) line_tail
      | '<' -> syntax_error_score ('>' :: stack) line_tail
      | c -> (
          match stack with
          | stack_head :: stack_tail when stack_head = c ->
              syntax_error_score stack_tail line_tail
          | _ -> Either.left (illegal_score c)))

let day10a i =
  let lines = In_channel.input_lines i in
  List.map (syntax_error_score []) lines
  |> List.filter_map Either.find_left
  |> ListExt.sum

let day10b i =
  let lines = In_channel.input_lines i in
  let scores =
    List.map (syntax_error_score []) lines
    |> List.filter_map Either.find_right
    |> Array.of_list
  in
  Array.sort compare scores;
  scores.(Array.length scores / 2)
