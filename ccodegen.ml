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
  let prefix = "_ha_" in

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
    | A.List(_) -> raise (Failure "ctype_of_typ: Lists")
    | A.Array(t) ->
       let (spec, declt) = ctype_of_typ t in
       (spec, C.PTR ([], declt))
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
    (* TODO: FuncType *)
    | A.FuncType(_) -> raise (Failure "ctype_of_typ: FuncType")
    | _ -> raise (Failure "ctype_of_typ: not yet implemented")
  in

  let rec expr env = function
      A.IntLit i -> (env, C.CONSTANT (C.CONST_INT (string_of_int i)))
    | A.BoolLit b -> (env, C.CONSTANT (C.CONST_INT (if b then "1" else "0")))
    | A.StringLit s -> (env, C.CONSTANT (C.CONST_STRING s))
    | A.FloatLit f -> (env, C.CONSTANT (C.CONST_FLOAT (string_of_float f)))
    | A.TupleLit _ -> raise (Failure "expr: TUPLELIT")
    | A.ListLit _ -> raise (Failure "expr: LISTLIT")
    | A.Id id -> 
       begin match id with
         A.NaiveId n -> (env, C.VARIABLE n)
       | A.MemberId (id, field) -> 
          let (env', container) = expr env (A.Id id) in
          (env', C.MEMBEROFPTR (container, field))
       | A.IndexId (id, index_expr) -> 
          let (env', index) = expr env index_expr in
          let (env'', container) = expr env' (A.Id id) in
          (env'', C.INDEX (container, index))
       end
    | A.Binop (e1, bop, e2) ->
       let (env, ce1) = expr env e1 in
       let (env, ce2) = expr env e2 in
       let cbop = match bop with
           A.Add -> C.ADD
         | A.Sub -> C.SUB
         | A.Mult -> C.MUL
         | A.Div -> C.MOD
         | A.Equal -> C.EQ
         | A.Neq -> C.NE
         | A.Less -> C.LT
         | A.Leq -> C.LE
         | A.Greater -> C.GT
         | A.Geq -> C.GE
         | A.And -> C.AND
         | A.Or -> C.OR
       in
       (env, C.BINARY (cbop, ce1, ce2))
    | A.Unop (uop, e) ->
       let (env, ce) = expr env e in
       let cuop = match uop with
           A.Neg -> C.MINUS
         | A.Not -> C.NOT
       in
       (env, C.UNARY (cuop, ce))
    | A.Assign (id, e) ->
       let (env, ce1) = expr env (A.Id id) in
       let (env, ce2) = expr env e in
       (env, C.BINARY (C.ASSIGN, ce1, ce2))
    (* TODO: there should probably be something different for lambdas *)
    | A.Call (id, elist) -> 
       let fid = match id with
           A.NaiveId n -> A.NaiveId (prefix ^ n)
         | _ -> id
       in
       let (env, caller) = expr env (A.Id fid) in
       let (env, callee) = List.fold_left 
                             (fun (env', acc) e ->
                               let (env'', ce) = expr env' e in
                               (env'', ce :: acc))
                             (env, [])
                             elist
       in
       (env, C.CALL (caller, callee))
    | A.Lambda _ -> raise (Failure "expr: LAMBDA")
    | A.Null -> (env, C.CONSTANT (C.CONST_INT "0"))
    | A.Noexpr -> (env, C.NOTHING)
  in

  let rec stmt env = function
      A.Block sl -> 
      let (env', cslist) = stmt_list env sl in
      let cblock = { C.blabels = [];
                     C.battrs  = [];
                     C.bstmts  = cslist } in
      (env', C.BLOCK (cblock, stubloc))
    | A.Expr e -> 
       let (env', cexpr) = expr env e in
       (env', C.COMPUTATION (cexpr, stubloc))
    | A.Return e ->
       let (env', cexpr) = expr env e in
       (env', C.RETURN (cexpr, stubloc))
    | A.If (e, st1, st2) ->
       let (env1, cexpr) = expr env e in
       let (env2, cst1)  = stmt env1 st1 in
       let (env3, cst2)  = stmt env2 st2 in
       ({env with program = env3.program}, 
        C.IF (cexpr, cst1, cst2, stubloc))
    | A.For (e1, e2, e3, st) ->
       let (env1, ce1) = expr env e1 in
       let (env2, ce2) = expr env1 e2 in
       let (env3, ce3) = expr env2 e3 in
       let (env4, cst) = stmt env3 st in
       ({env with program = env4.program},
       C.FOR (C.FC_EXP ce1, ce2, ce3, cst, stubloc))
    | A.While (e, st) ->
       let (env1, ce) = expr env e in
       let (env2, cst) = stmt env1 st in
       ({env with program = env2.program},
       C.WHILE (ce, cst, stubloc))
    | A.Local vd ->
       begin match vd with
         A.Bind (t, id) ->
         let (spec, declt) = ctype_of_typ t in
         let def = C.DECDEF ((spec, [((id, declt, [], stubloc), C.NO_INIT)]), stubloc) in
         (env, C.DEFINITION def)
       | A.Binass(t, id, e) ->
         let (spec, declt) = ctype_of_typ t in
         let (env', cexp) = expr env e in
         let def = C.DECDEF ((spec, [((id, declt, [], stubloc), C.SINGLE_INIT cexp)]), stubloc) in
         (env', C.DEFINITION def)
       end

  (* Returns (env, list of C statements) *)
  and stmt_list env slist = 
    let (env', cslist) = List.fold_left (fun (e, acc) s ->
                                          let (e', cstmt) = stmt e s in
                                          (e', cstmt :: acc))
                                        (env, [])
                                        slist
    in
    (env', List.rev cslist)
  in

  let func env f =
    let (ret_spec, ret_declt) = ctype_of_typ f.A.typ in
    let bind_to_single_name (bt, bname) =
      let (spec, declt) = ctype_of_typ bt in
      let sname = (bname, declt, [], stubloc) in
      (spec, sname)
    in
    (* NOTE: last parameter to C.PROTO indicates if this is a varg function *)
    let fdeclt = C.PROTO (ret_declt,
                          List.map bind_to_single_name f.A.formals, 
                          false) in
    let fname = if f.A.fname = "main" then f.A.fname else prefix ^ f.A.fname in
    let fsingle_name = (ret_spec, (fname, fdeclt, [], stubloc)) in
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
      A.Typedef (t, id) ->
      let (spec, declt) = ctype_of_typ t in
      let def = C.DECDEF ((C.SpecTypedef :: spec, [((id, declt, [], stubloc), C.NO_INIT)]), stubloc) in
      {env with program = def :: env.program}
    | A.Global vd -> 
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
  in
  
  let genv = List.fold_left gstmt initial_env global_stmts in
  let func_env = List.fold_left func genv functions in
  
  (outfile, List.rev func_env.program)
