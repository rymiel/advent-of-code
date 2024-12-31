open Day18ex
open Aoclib.Util

let eval parser i =
  let eval_expr s =
    let lexbuf = Lexing.from_string s in
    parser Lexer.token lexbuf
  in
  ListExt.map_sum eval_expr (In_channel.input_lines i)

let day18a = eval Parser.part1
let day18b = eval Parser.part2
