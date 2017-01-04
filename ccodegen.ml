(* Generates C code for AST *)

module A = Ast
module C = Cabs

module StringMap = Map.Make(String)

let debug msg =
  if false
  then prerr_endline msg
  else ()

type environment = {
    (* MAYBE: don't need externals and locals anymore *)
    externals: C.expression StringMap.t;
    locals: C.expression StringMap.t;
    program: C.definition list
  }
                     
let translate (global_stmts, functions) =
  let generator = ref 0 in
  let nextint () =
    let i = !generator in
    generator := (i + 1);
    i
  in
  
  let outfile = "ccodegen.out" in
  let stubloc = { C.lineno = 1;
                  C.filename = outfile;
                  C.byteno = 0;
                  C.ident = 0 } in

  (* user-defined types *)
  (* let user_types = List.fold_left *)
  (*                    (fun map -> function *)
  (*                        A.Typedef(t, s) -> StringMap.add s t map *)
  (*                      | _ -> map) *)
  (*                    (StringMap.add "mutex" S.mutex_t StringMap.empty) *)
  (*                    global_stmts *)
  (* in *)

  (* Returns (specifier, decl_type) *)
  let rec ctype_of_typ =
    let ptr_t = C.PTR ([], C.JUSTBASE) in
    let ctype_of_spec tspec isptr =
      ([C.SpecType tspec], if isptr
                           then ptr_t
                           else C.JUSTBASE)
    in
    function
      A.DataType(A.Int) -> ctype_of_spec C.Tint false
    | A.DataType(A.Bool) -> ctype_of_spec C.Tbool false
    | A.DataType(A.Float) -> ctype_of_spec C.Tfloat false
    | A.DataType(A.Void) -> ctype_of_spec C.Tvoid false
    | A.DataType(A.String) -> ctype_of_spec C.Tchar true
    | A.Tuple(tlist) ->
       let typ_to_field_group index t =
         let (spec, declt) = ctype_of_typ t in
         let cname = ("_field_" ^ (string_of_int index), declt, [], stubloc) in
         (spec, [(cname, None)])
       in
       ctype_of_spec (C.Tstruct ("_tuple_" ^ string_of_int (nextint ()) ^ "_t",
                                 (if List.length tlist = 0
                                  then None
                                  else Some (List.mapi typ_to_field_group tlist)),
                                 []))
                     true
    (* TODO: implement lists = dynamic arrays *)
    (* TODO: rename this to array *)
    | A.List(t) ->
       let (spec, declt) = ctype_of_typ t in
       (spec, C.PTR ([], declt))
    (* TODO: channels *)
    | A.Struct(name, blist) ->
       let bind_to_field_group (t, fname) =
         let (spec, declt) = ctype_of_typ t in
         let cname = (fname, declt, [], stubloc) in
         (spec, [(cname, None)])
       in
       ctype_of_spec (C.Tstruct (name,
                                 (if List.length blist = 0
                                  then None
                                  else Some (List.map bind_to_field_group blist)),
                                 []))
                     true
    | A.UserType(n) -> ctype_of_spec (C.Tnamed n) false
    (* TODO *)
    | A.FuncType(_) -> raise (Failure "ctype_of_typ: haven't figured out how functions work yet")
    | _ -> raise (Failure "ctype_of_typ: not yet implemented")
  in

  let expr env = function
      A.IntLit i -> (env, C.CONSTANT (C.CONST_INT (string_of_int i)))
    | A.BoolLit b -> (env, C.CONSTANT (C.CONST_INT (if b then "1" else "0")))
    | A.StringLit s -> (env, C.CONSTANT (C.CONST_STRING s))
    | A.FloatLit f -> (env, C.CONSTANT (C.CONST_FLOAT (string_of_float f)))
    | _ -> raise (Failure "expr: not yet implemented")
  in

  let rec stmt env = function
      A.Block sl -> 
      let (env', cslist) = stmt_list env sl in
      let cblock = { C.blabels = [];
                     C.battrs  = [];
                     C.bstmts  = cslist } in
      (env', C.BLOCK (cblock, stubloc))
    | _ -> raise (Failure "stmt: not yet implemented")

  (* Returns (env, list of C statements) *)
  and stmt_list env slist = 
    List.fold_left (fun (e, acc) s ->
                     let (e', cstmt) = stmt e s in
                     (e', cstmt :: acc))
                   (env, [])
                   slist
  in

  let func env f =
    (* TODO: find out where ret_declt fits in *)
    let (ret_spec, _) = ctype_of_typ f.A.typ in
    let bind_to_single_name (bt, bname) =
      let (spec, declt) = ctype_of_typ bt in
      let sname = (bname, declt, [], stubloc) in
      (spec, sname)
    in
    (* NOTE: last parameter to C.PROTO indicates if this is a varg function *)
    let fdeclt = C.PROTO (C.JUSTBASE, 
                          List.map bind_to_single_name f.A.formals, 
                          false) in
    let fsingle_name = (ret_spec, (f.A.fname, fdeclt, [], stubloc)) in
    (* MAYBE: update env.externals and locals here *)
    let (env', cslist) = stmt_list env f.A.body in
    let cblock = { C.blabels = [];
                   C.battrs  = [];
                   C.bstmts  = cslist } in
    let cfunc = C.FUNDEF (fsingle_name, cblock, stubloc, stubloc) in
    { env' with program = cfunc :: env'.program }
  in

  let initial_env = {
      externals = StringMap.empty;
      locals = StringMap.empty;
      program = [C.DIRECTIVE(C.INCLUDE("<stdio.h>"), stubloc)] }
  in

  let gstmt env = function
      A.Global(vd) -> 
      begin match vd with
        A.Bind(t, id) ->
        let (spec, declt) = ctype_of_typ t in
        let def = C.DECDEF ((spec, [((id, declt, [], stubloc), C.NO_INIT)]), stubloc) in
        {env with program = def :: env.program}
      | A.Binass(t, id, e) -> 
         let (spec, declt) = ctype_of_typ t in
         let (env', cexp) = expr env e in
         let def = C.DECDEF ((spec, [((id, declt, [], stubloc), C.SINGLE_INIT cexp)]), stubloc) in
         {env' with program = def :: env'.program}
      end
    | _ -> env
  in
  
  let genv = List.fold_left gstmt initial_env global_stmts in
  let func_env = List.fold_left func genv functions in
  
  (outfile, List.rev func_env.program)
