(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or 

type uop = Neg | Not

type primitive = Int | Bool | Void | String | Float | Unknown

type typ = 
    DataType of primitive 
  | Tuple of typ list
  | List of typ
  | Channel of typ
  | Struct of string * (typ * string) list
  | UserType of string
  | FuncType of typ list

type id = 
    NaiveId of string
  | MemberId of id * string
  | IndexId of id * expr

and expr =
    IntLit of int
  | BoolLit of bool
  | StringLit of string
  | FloatLit of float
  | TupleLit of expr list
  | ListLit of expr list
  | Id of id
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of id * expr
  | Call of id * expr list
  | Noexpr

type var_decl = 
    Bind of typ * string
  | Binass of typ * string * expr

type global_stmt =
    Typedef of typ * string
  | Global of var_decl

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Local of var_decl

type func_decl = {
    typ : typ;
    fname : string;
    formals : (typ * string) list;
    body : stmt list;
  }

type program = global_stmt list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"
  | Div -> "Div"
  | Equal -> "Equal"
  | Neq -> "Neq"
  | Less -> "Less"
  | Leq -> "Leq"
  | Greater -> "Greater"
  | Geq -> "Geq"
  | And -> "And"
  | Or -> "Or"


let string_of_uop = function
    Neg -> "Neg"
  | Not -> "Not"


let rec string_of_id = function
    NaiveId(s) -> s
  | MemberId(id, s) -> "Member(" ^ string_of_id id ^ ", " ^ s ^ ")"
  | IndexId(id, e) -> "Index(" ^ string_of_id id ^ ", " ^ string_of_expr e ^ ")"

and string_of_expr = function
    IntLit(l) -> "IntLit(" ^ string_of_int l ^ ")"
  | BoolLit(true) -> "BoolLit(True)"
  | BoolLit(false) -> "BoolLit(False)"
  | StringLit(s) -> "StringLit(\"" ^ s ^ "\")"
  | FloatLit(f) -> "FloatLit(" ^ string_of_float f ^ ")"
  | TupleLit(elist) -> "TupleLit(" ^ String.concat ", " (List.map string_of_expr elist) ^ ")"
  | ListLit(elist) -> "ListLit(" ^ String.concat ", " (List.map string_of_expr elist) ^ ")"
  | Id(s) -> "Id(" ^ string_of_id s ^ ")"
  | Binop(e1, o, e2) ->
      "Binop(" ^ string_of_expr e1 ^ ", " ^ string_of_op o ^ ", " ^ string_of_expr e2 ^ ")"
  | Unop(o, e) -> "Unop(" ^ string_of_uop o ^ ", " ^ string_of_expr e ^ ")"
  | Assign(v, e) -> "Assign(" ^ string_of_id v ^ ", " ^ string_of_expr e ^ ")"
  | Call(f, el) ->
     "Call(" ^ string_of_id f ^ ", " ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> "Noexpr"


let rec string_of_typ = function
    DataType(Int) -> "Int"
  | DataType(Bool) -> "Bool"
  | DataType(Void) -> "Void"
  | DataType(Float) -> "Float"
  | DataType(String) -> "String"
  | DataType(Unknown) -> "Unknown"
  | Tuple(tlist) -> "Tuple(" ^ String.concat ", " (List.map string_of_typ tlist) ^ ")"
  | List(t) -> "List(" ^ string_of_typ t ^ ")"
  | Struct(id, vlist) -> "Struct(" ^ id ^ ", " ^ String.concat "" (List.map string_of_bind vlist) ^ ")"
  | Channel(t) -> "Channel(" ^ string_of_typ t ^ ")"
  | UserType(id) -> "UserType(" ^ id ^ ")"
  | FuncType(tlist) -> "FuncType(" ^ String.concat ", " (List.map string_of_typ tlist) ^ ")"

and string_of_bind (t, id) = "Bind(" ^ string_of_typ t ^ ", " ^ id ^ ")\n"

let string_of_vdecl = function
    Bind(t, s) -> "Bind(" ^ string_of_typ t ^ ", " ^ s ^ ")\n"
  | Binass(t, s, e) -> "Binass(" ^ string_of_typ t ^ ", " ^ s ^ ", " ^ string_of_expr e ^ ")\n"

let string_of_global_stmt = function
    Typedef(t, s) -> "Typedef(" ^ string_of_typ t ^ ", " ^ s ^ ")\n"
  | Global(vd) -> string_of_vdecl vd

let rec string_of_stmt = function
    Block(stmts) ->
      "Block(\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ ")\n"
  | Expr(expr) -> "Expr(" ^ string_of_expr expr ^ ")\n";
  | Return(expr) -> "Return(" ^ string_of_expr expr ^ ")\n";
  | If(e, s, Block([])) -> "If(" ^ string_of_expr e ^ ", " ^ string_of_stmt s ^ ")\n"
  | If(e, s1, s2) ->  "If(" ^ string_of_expr e ^ ", " ^
      string_of_stmt s1 ^ ", " ^ string_of_stmt s2 ^ ")\n"
  | For(e1, e2, e3, s) ->
      "For(" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ", " ^ string_of_stmt s ^ ")\n"
  | While(e, s) -> "While(" ^ string_of_expr e ^ ", " ^ string_of_stmt s ^ ")\n"
  | Local(vd) -> string_of_vdecl vd

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  "FuncName(" ^ fdecl.fname ^ ")" ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (gstmts, funcs) =
  String.concat "" (List.map string_of_global_stmt gstmts) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
