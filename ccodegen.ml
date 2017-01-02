(* Generates C code for AST *)

module A = Ast
module S = Semant
module C = Cabs

module StringMap = Map.Make(String)

let vcount = ref 0              (* # of variables used *)

let debug msg =
  if false
  then prerr_endline msg
  else ()

let translate (global_stmts, functions) =

  ignore global_stmts;
  ignore functions;
  
  (* let rec ctype_of_typ = function *)
  (*     A.DataType(A.Int) -> [C.SpecType C.Int] *)
  (*   | A.DataType(A.Bool) -> i1_t *)
  (*   | A.DataType(A.Float) -> dbl_t *)
  (*   | A.DataType(A.Void) -> void_t *)
  (*   | A.DataType(A.String) -> string_t *)
  (*   | A.Tuple(tlist) -> L.struct_type context (Array.of_list (List.map ltype_of_typ tlist)) *)
  (*   (\* TODO: implement dynamic arrays *\) *)
  (*   | A.List(t) -> L.pointer_type (ltype_of_typ t) *)
  (*   (\* TODO: channels *\) *)
  (*   | A.Struct(name, blist) -> *)
  (*      let t =  *)
  (*        (try Hashtbl.find typ_cache name *)
  (*         with Not_found ->  *)
  (*           let struct_t = L.named_struct_type context name in *)
  (*           Hashtbl.add typ_cache name struct_t; *)
  (*           L.struct_set_body struct_t (Array.of_list (List.map ltype_of_typ (List.map fst blist))) false; *)
  (*           struct_t) in *)
  (*      L.pointer_type t *)
  (*   | A.UserType(_) as t -> let t' = S.resolve_user_type t user_types in *)
  (*                           ltype_of_typ t' *)
  (*   | A.FuncType(tlist) -> *)
  (*      let ftype =  *)
  (*        let llist = List.map ltype_of_typ tlist in *)
  (*        let return_t = List.hd llist in *)
  (*        let params_t = Array.of_list (List.tl llist) in *)
  (*        L.function_type return_t params_t *)
  (*      in *)
  (*      L.pointer_type ftype *)
  (*   | _ -> raise (Failure "Not yet implemented") *)
  (* in *)

  ("ccodegen.out", [])
