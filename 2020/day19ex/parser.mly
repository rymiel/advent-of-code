%token <int> INT
%token <char> CHR
%token COLON PIPE QUOTE EOF
%start <Data.rule> rule
%type <int list> elist
%%
rule: i = INT; COLON; e = expr; EOF { (i, e) };
expr:
  QUOTE; c = CHR; QUOTE { Chr c }
| l = elist { List l }
| a = elist; PIPE; b = elist { Alternate (a, b) }
;

elist: l = list(INT) { l };