{
open Parser
exception Eof
}
rule token = parse
  '\n'          { LF }
| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| ','           { COMMA }
| '['           { LBRACKET }
| ']'           { RBRACKET }
| eof           { EOF }