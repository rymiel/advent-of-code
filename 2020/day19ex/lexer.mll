{
open Parser
exception Eof
}
rule token = parse
  [' ' '\t'] { token lexbuf }
| ['0'-'9']+ as i
  { INT(int_of_string i) }
| ['a'-'z'] as c
  { CHR(c) }
| ':' { COLON }
| '|' { PIPE }
| '"' { QUOTE }
| eof { EOF }