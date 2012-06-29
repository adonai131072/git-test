%{
  open Def
%}

%token <string> VAR
%token <int> INT
%token PLUS MINUS MUL DIV
%token LPAREN RPAREN
%token LBRK RBRK ESC RUN
%token ARROW
%token EQ
%token FUN
%token RFUN
%token LET
%token REC
%token IN
%token IFZ
%token THEN
%token ELSE
%token EOF

%nonassoc ARROW IN ELSE
%left EQ
%left PLUS MINUS
%left MUL DIV
%left VAR INT LPAREN LBRK
%nonassoc ESC RUN
%start main
%type <Def.expr> main

%%

main:
  expr EOF { $1 }
;
args:
    INT { Int $1 }
  | VAR { Var $1 }
  | LPAREN expr RPAREN { $2 }
  | LBRK expr RBRK { Brk($2) }
;
expr:
    INT { Int $1 }
  | VAR { Var $1 }
  | expr args { App($1, $2) }
  | LPAREN expr RPAREN { $2 }
  | expr PLUS expr { Prim2("+", $1, $3) }
  | expr MINUS expr { Prim2("-", $1, $3) }
  | expr MUL expr { Prim2("*", $1, $3) }
  | expr DIV expr { Prim2("/", $1, $3) }
  | LET VAR EQ expr IN expr { Let($2, $4, $6) }
  | FUN VAR ARROW expr { Fun($2, TInt, $4) }
  | RFUN VAR VAR ARROW expr { RFun($2, $3, TInt, TInt, $5) }
  | IFZ expr THEN expr ELSE expr { Ifz($2, $4, $6) }
  | LBRK expr RBRK { Brk($2) }
  | ESC expr { Esc($2) }
  | RUN expr { Run($2) }
;
