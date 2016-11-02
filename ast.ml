(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Member

type uop = Neg | Not

type typ = 
    Int
  | Bool
  | Void
  | String
  | Float
  | Tuple of typ list
  | List of typ
  | Channel of typ
  | Struct of string * (typ * string) list
  | UserType of string
  | FuncType of typ list

type bind = typ * string

type expr =
    Literal of int
  | BoolLit of bool
  | StringLit of string
  | FloatLit of float
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Typedef of typ * string
  | Bind of typ * string

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

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
  | Member -> "Member"

let string_of_uop = function
    Neg -> "Neg"
  | Not -> "Not"

let rec string_of_expr = function
    Literal(l) -> "Literal(" ^ string_of_int l ^ ")"
  | BoolLit(true) -> "BoolLit(True)"
  | BoolLit(false) -> "BoolLit(False)"
  | StringLit(s) -> "StringLit(\"" ^ s ^ "\")"
  | FloatLit(f) -> "FloatLit(" ^ string_of_float f ^ ")"
  | Id(s) -> "Id(" ^ s ^ ")"
  | Binop(e1, o, e2) ->
      "Binop(" ^ string_of_expr e1 ^ ", " ^ string_of_op o ^ ", " ^ string_of_expr e2 ^ ")"
  | Unop(o, e) -> "Unop(" ^ string_of_uop o ^ ", " ^ string_of_expr e ^ ")"
  | Assign(v, e) -> "Assign(" ^ v ^ ", " ^ string_of_expr e ^ ")"
  | Call(f, el) ->
     "Call(" ^ f ^ ", " ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> "Noexpr"

let rec string_of_typ = function
    Int -> "Int"
  | Bool -> "Bool"
  | Void -> "Void"
  | Float -> "Float"
  | String -> "String"
  | Tuple(tlist) -> "Tuple(" ^ String.concat ", " (List.map string_of_typ tlist) ^ ")"
  | List(t) -> "List(" ^ string_of_typ t ^ ")"
  | Struct(id, vlist) -> "Struct(" ^ id ^ ", " ^ String.concat "" (List.map string_of_vdecl vlist) ^ ")"
  | Channel(t) -> "Channel(" ^ string_of_typ t ^ ")"
  | UserType(id) -> "UserType(" ^ id ^ ")"
  | FuncType(tlist) -> "FuncType(" ^ String.concat ", " (List.map string_of_typ tlist) ^ ")"

and string_of_vdecl (t, id) = "Vdecl(" ^ string_of_typ t ^ ", " ^ id ^ ")\n"

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
  | Typedef(t, s) -> "Typedef(" ^ string_of_typ t ^ ", " ^ s ^ ")\n"
  | Bind(t, s) -> "Bind(" ^ string_of_typ t ^ ", " ^ s ^ ")\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  "FuncName(" ^ fdecl.fname ^ ")" ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
