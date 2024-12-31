%token <int> INT
%token LPAREN RPAREN ADD MUL EOF
%left MUL
%left ADD
%left LEVEL
%start <int> part1
%start <int> part2
%%
part1: e = expr1; EOF { e }
expr1:
    i = INT { i }
  | LPAREN; e = expr1; RPAREN { e }
  | a = expr1; ADD; b = expr1 %prec LEVEL { a + b }
  | a = expr1; MUL; b = expr1 %prec LEVEL { a * b }

part2: e = expr2; EOF { e }
expr2:
    i = INT { i }
  | LPAREN; e = expr2; RPAREN { e }
  | a = expr2; ADD; b = expr2 { a + b }
  | a = expr2; MUL; b = expr2 { a * b }
;