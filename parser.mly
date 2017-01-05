/* Ocamlyacc parser for Harmonica */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA QUOTE DOT
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token CPLUS CMINUS
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT FLOAT BOOL STRING VOID TUPLE LIST ARRAY STRUCT_STMT TYPEDEF
%token CHANNEL PARALLEL CHAN LAMBDA SIZEOF
%token NULL
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> ID STRING_LITERAL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right CPLUS CMINUS
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG
%left DOT

%start program
%type <Ast.program> program

%%

program:
  decls EOF { List.rev (fst $1), List.rev (snd $1) }

decls:
   /* nothing */ { [], [] }
 | decls global { ($2 :: fst $1), snd $1 }
 | decls fdecl  { fst $1, ($2 :: snd $1) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

tuple_typ_list:
    /* nothing */ { [] }
  | tuple_typ_list typ { $2 :: $1 }

func_typ_list:
    typ { [$1] }
  | func_typ_list typ { $2 :: $1 }

primitive:
    INT { Int }
  | BOOL { Bool }
  | VOID { Void }
  | STRING { String }
  | FLOAT { Float }

typ:
    primitive  { DataType($1) }
  | TUPLE LBRACKET tuple_typ_list RBRACKET { Tuple(List.rev $3) }
  | ARRAY LBRACKET typ RBRACKET { Array($3) }
  | LIST LBRACKET typ RBRACKET { List($3) }
  | CHANNEL LBRACKET typ RBRACKET { Channel($3) }
  | ID { UserType($1) }
  | LT func_typ_list GT { FuncType((List.hd $2) :: List.rev (List.tl $2)) }

vdecl:
    typ ID SEMI { Bind($1, $2) }
  | typ ID ASSIGN expr SEMI { Binass($1, $2, $4) }

global:
    STRUCT_STMT ID LBRACE bind_list RBRACE SEMI { Typedef(Struct($2, List.rev $4), $2) }
  | TYPEDEF typ ID SEMI { Typedef($2, $3) }
  | vdecl { Global($1) }

bind_list:
    /* nothing */ { [] }
  | bind_list bind { $2 :: $1 }

bind:
    typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { if $5 = Noexpr
       then For($3, BoolLit(true), $7, $9) 
       else For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | vdecl { Local($1) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr_list_opt:
    /* nothing */ { [] }
  | expr_list { List.rev $1 }

expr_list:
    expr { [$1] }
  | expr_list COMMA expr { $3 :: $1 }

expr_comma_list_opt:
    /* nothing */ { [] }
  | expr COMMA { [$1] }
  | expr_comma_list { List.rev $1 }

expr_comma_list:
    expr COMMA expr { $3 :: [$1] }
  | expr_comma_list COMMA expr { $3 :: $1 }

id_expr:
    ID                 { NaiveId($1) }
  | id_expr  DOT  ID   { MemberId($1, $3) }
  | id_expr  LBRACKET expr RBRACKET { IndexId($1, $3) }

expr:
    literals          { $1 }
  | NULL              { Null }
  | id_expr               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | id_expr CPLUS expr  { Assign($1, Binop(Id($1), Add, $3))}
  | id_expr CMINUS expr { Assign($1, Binop(Id($1), Sub, $3))}
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | id_expr ASSIGN expr   { Assign($1, $3) }
  | id_expr LPAREN actuals_opt RPAREN { Call($1, $3) }
  | PARALLEL LPAREN actuals_opt RPAREN { Call(NaiveId("parallel"), $3) }
  | CHAN LPAREN chan_actuals_opt RPAREN { Call(NaiveId("chan"), $3) }
  | LAMBDA LPAREN formals_opt RPAREN typ LPAREN expr RPAREN { Lambda($5, $3, $7) }
  | SIZEOF LPAREN typ RPAREN { SizeofTyp($3) }
  | LPAREN expr RPAREN { $2 }

primitives:
    INT_LITERAL      { IntLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | FLOAT_LITERAL    { FloatLit($1) }
  | STRING_LITERAL   { StringLit($1) }

literals:
    primitives         { $1 }
  | LPAREN expr_comma_list_opt RPAREN { TupleLit($2) }
  | LBRACKET expr_list_opt RBRACKET { ListLit($2) }

chan_actuals_opt:
    typ { [StringLit(string_of_typ $1)] }
  | typ COMMA expr { StringLit(string_of_typ $1) :: [$3] }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
