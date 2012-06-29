{
  open Parser
  exception Eof
}

let space = [' ' '\t' '\n' '\r']

rule token = parse
  | ['0' - '9']+ as lxm { INT (int_of_string lxm) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_''\'']* as id
      { match id with
          | "fun" -> FUN
	  | "rfun" -> RFUN
          | "let" -> LET
          | "rec" -> REC
	  | "in" -> IN
	  | "ifz" -> IFZ
	  | "then" -> THEN
	  | "else" -> ELSE
	  | _ -> VAR id
      }
  | "->" { ARROW }
  | ".<" { LBRK }
  | ">." { RBRK }
  | ".~" { ESC }
  | ".!" { RUN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MUL }
  | '/' { DIV }
  | '=' { EQ }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | space+ { token lexbuf }
  | eof { EOF }
