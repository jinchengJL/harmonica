(* Ocamllex scanner for MicroC *)

{ 
	open Parser 
}

let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let str = (ascii | escape)*

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "#"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '\"'     { QUOTE }
| '\''     { QUOTE }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "."      { DOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
(* type keywords *)
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "string" { STRING }
| "void"   { VOID }
| "tuple"  { TUPLE }
| "list"   { LIST }
| "struct" { STRUCT_STMT }
| "true"   { TRUE }
| "false"  { FALSE }
| "typedef" { TYPEDEF }
| "channel" { CHANNEL }
| "chan"    { CHAN }
| "parallel" { PARALLEL }

(* string literal *)
| '"' (str as lxm) '"'      { STRING_LITERAL(lxm) }
(* int literal *)
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
(* float literal *)
| ['-''+']?['0'-'9']*'.'?['0'-'9']+(['e''E']['-''+']?['0'-'9']+)? as lxm { FLOAT_LITERAL(float_of_string lxm) }
(* ID *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "\n" { token lexbuf }
| _    { comment lexbuf }
