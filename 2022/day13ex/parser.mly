%token <int> INT
%token LBRACKET RBRACKET COMMA LF EOF
%start <Data.data> main
%{
    open Data
%}
%%
main: l = separated_list(LF, npair) EOF { l };
npair: a = node; LF; b = node; LF { (a, b) };
node:
    i = INT { Num i }
    | LBRACKET; l = separated_list(COMMA, node); RBRACKET { List l }
;