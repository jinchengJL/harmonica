(* Generates C code for AST *)

module A = Ast
module S = Semant

module StringMap = Map.Make(String)

let vcount = ref 0              (* # of variables used *)

let debug msg =
  if false
  then prerr_endline msg
  else ()

let translate (global_stmts, functions) =
  ()
