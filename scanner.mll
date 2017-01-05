(* Ocamllex scanner for Harmonica *)

{ 
	open Parser 

}

let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let str = (ascii | escape)*

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }  (* Whitespace *)
| "#"      { comment lexbuf }            (* Comments *)
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
| "+="     { CPLUS }
| "-="     { CMINUS }
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
| "array"  { ARRAY }
| "struct" { STRUCT_STMT }
| "true"   { TRUE }
| "false"  { FALSE }
| "typedef" { TYPEDEF }
| "channel" { CHANNEL }
| "chan"    { CHAN }
| "parallel" { PARALLEL }
| "lambda"  { LAMBDA }
| "sizeof" { SIZEOF }
| "NULL"   { NULL }

(* string literal *)
| '"' (([' '-'!' '#'-'&' '('-'[' ']'-'~'] | '\\' [ '\\' '"' 'n' 'r' 't' '''])* as lxm) '"'  { STRING_LITERAL(lxm) }
(* int literal *)
| ['0'-'9']+ as lxm { INT_LITERAL(int_of_string lxm) }
(* float literal *)
| ['-''+']?['0'-'9']*('.')['0'-'9']+(['e''E']['-''+']?['0'-'9']+)? as lxm { FLOAT_LITERAL(float_of_string lxm) }
(* ID *)
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "\n" { token lexbuf }
| _    { comment lexbuf }

(*
and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Failure("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Failure("String is not terminated")) }*)
