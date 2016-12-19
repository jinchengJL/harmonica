(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

 *)

module L = Llvm
module A = Ast
module S = Semant

module StringMap = Map.Make(String)

type environment = {
    externals: L.llvalue StringMap.t;
    locals: L.llvalue StringMap.t;
    builder: L.llbuilder;
  }

let debug msg =
  if false
  then prerr_endline msg
  else ()

let translate (global_stmts, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Harmonica"
  and i32_t  = L.i32_type    context
  and i8_t   = L.i8_type     context
  and i1_t   = L.i1_type     context
  and dbl_t  = L.double_type context
  and void_t = L.void_type   context in
  let string_t = L.pointer_type i8_t in
  let voidstar_t = string_t in

  (* user-defined types *)
  let user_types = List.fold_left
                     (fun map -> function
                         A.Typedef(t, s) -> StringMap.add s t map
                       | _ -> map)
                     (StringMap.add "mutex" S.mutex_t StringMap.empty)
                     global_stmts
  in

  (* map of struct names to their fields *)
  let struct_map = List.fold_left
                     (fun map -> function
                         A.Typedef(A.Struct(name, bind_list), _) ->
                         StringMap.add name bind_list map
                       | _ -> map)
                     StringMap.empty
                     global_stmts
  in

  let typ_cache = ref StringMap.empty in

  let rec ltype_of_typ = function
      A.DataType(A.Int) -> i32_t
    | A.DataType(A.Bool) -> i1_t
    | A.DataType(A.Float) -> dbl_t
    | A.DataType(A.Void) -> void_t
    | A.DataType(A.String) -> string_t
    | A.Tuple(tlist) -> L.struct_type context (Array.of_list (List.map ltype_of_typ tlist))
    (* TODO: implement dynamic arrays *)
    | A.List(t) -> L.pointer_type (ltype_of_typ t)
    (* TODO: channels *)
    | A.Struct(name, blist) ->
       let t = 
         (try StringMap.find name !typ_cache
          with Not_found -> 
            let struct_t = L.named_struct_type context name in
            L.struct_set_body struct_t (Array.of_list (List.map ltype_of_typ (List.map fst blist))) false;
            typ_cache := StringMap.add name struct_t !typ_cache;
            struct_t) in
       L.pointer_type t
    | A.UserType(_) as t -> let t' = S.resolve_user_type t user_types in
                            ltype_of_typ t'
    | A.FuncType(tlist) ->
       let ftype = 
         let llist = List.map ltype_of_typ tlist in
         let return_t = List.hd llist in
         let params_t = Array.of_list (List.tl llist) in
         L.function_type return_t params_t
       in
       L.pointer_type ftype
    | _ -> raise (Failure "Not yet implemented")
  in

  (* let rec typ_of_ltype ltype = match (L.classify_type ltype) with *)
  (*     L.TypeKind.Integer -> if ltype = i1_t *)
  (*                           then A.DataType(A.Bool)  *)
  (*                           else A.DataType(A.Int) *)
  (*   | L.TypeKind.Double -> A.DataType(A.Float) *)
  (*   | L.TypeKind.Void -> A.DataType(A.Void) *)
  (*   | L.TypeKind.Pointer -> A.DataType(A.String) *)
  (*   | L.TypeKind.Array -> A.List (typ_of_ltype (L.element_type ltype)) *)
  (*   | L.TypeKind.Struct ->  *)
  (*      (match (L.struct_name ltype) with *)
  (*         None -> A.Tuple (List.map typ_of_ltype (Array.to_list (L.struct_element_types ltype))) *)
  (*       | Some(name) -> A.Struct (name, StringMap.find name struct_map)) *)
  (*   | L.TypeKind.Function -> *)
  (*      let return_t = L.return_type ltype in *)
  (*      let param_ts = Array.to_list (L.param_types ltype) in *)
  (*      A.FuncType (List.map typ_of_ltype (List.rev (return_t :: (List.rev param_ts)))) *)
  (*   | _ -> raise (Failure "Unsupported llvm type") *)
  (* in *)
  
  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let str_concat_t = L.function_type string_t [| string_t; string_t|] in
  let str_concat_func = L.declare_function "str_concat" str_concat_t the_module in
  
  let subroutine_t = L.pointer_type (L.function_type voidstar_t [| voidstar_t |] ) in  
  let parallel_t = L.function_type i32_t [| subroutine_t; L.pointer_type voidstar_t; i32_t; i32_t |] in
  let parallel_func = L.declare_function "parallel" parallel_t the_module in

  let mutex_t = voidstar_t in
  let mutex_create_t = L.function_type mutex_t [||] in
  let mutex_create_func = L.declare_function "mutex_create" mutex_create_t the_module in
  
  let mutex_lock_t = L.function_type i32_t [| mutex_t |] in
  let mutex_lock_func = L.declare_function "lock" mutex_lock_t the_module in

  let mutex_unlock_t = L.function_type i32_t [| mutex_t |] in
  let mutex_unlock_func = L.declare_function "unlock" mutex_unlock_t the_module in

  let mutex_destroy_t = L.function_type i32_t [| mutex_t |] in
  let mutex_destroy_func = L.declare_function "destroy" mutex_destroy_t the_module in  

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname in
      let formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      let fval  = L.define_function name ftype the_module in
      StringMap.add name (fval, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  let (main_function, _) = StringMap.find "main" function_decls in
  let global_builder = L.builder_at_end context (L.entry_block main_function) in

  let int_format_str = L.build_global_stringptr "%d\n" "fmt" global_builder in
  let float_format_str = L.build_global_stringptr "%f\n" "fmt" global_builder in
  let endl_str = L.build_global_stringptr "\n" "fmt" global_builder in

  let llstore lval laddr builder =
    let ptr = L.build_pointercast laddr (L.pointer_type (L.type_of lval)) "" builder in
    let store_inst = (L.build_store lval ptr builder) in
    debug ((L.string_of_llvalue store_inst));
    ()
  in

  (* Return the address of a variable or formal argument *)
  let rec lookup env x = 
    begin match x with
      A.NaiveId(n) -> 
      let result = 
        (try StringMap.find n env.locals
         with Not_found -> 
           (try StringMap.find n env.externals
            with Not_found -> raise (Failure ("undeclared variable " ^ n))))
      in
      (* debug ("lookup: " ^ n ^ " = " ^ (L.string_of_llvalue result)); *)
      result
    | A.MemberId(id, field_name) -> 
       let rec find_index_of field_name list = 
         match list with
           [] -> raise (Failure ("undeclared field " ^ field_name))
         | hd :: tl -> if field_name = (snd hd) 
                       then 0 
                       else 1 + find_index_of field_name tl
       in
       let container_pp = lookup env id in
       let container_addr = L.build_load container_pp "" env.builder in
       let container = L.build_load container_addr "" env.builder in
       debug ("container: " ^ L.string_of_llvalue container);
       let container_tname_opt = L.struct_name (L.type_of container) in
       (match container_tname_opt with
          None -> raise (Failure ("expected struct, found tuple: " ^ A.string_of_id id))
        | Some(container_tname) ->
           let fields = StringMap.find container_tname struct_map in
           let idx = find_index_of field_name fields in
           let addr = L.build_struct_gep container_addr idx "" env.builder in
           addr)
    | A.IndexId(id, e) ->
       let array_pp = lookup env id in (* ptr to ptr to array *)
       let array_ptr = L.build_load array_pp "" env.builder in
       let index = snd (expr env e) in
       debug ("index = " ^ (L.string_of_llvalue index));
       let eaddr = L.build_gep array_ptr [|index|] "" env.builder in
       debug ("eaddr = " ^ (L.string_of_llvalue eaddr));
       (* let etype = L.element_type (L.type_of array_ptr) in *)
       (* let ptr = L.build_pointercast eaddr (L.pointer_type etype) "" env.builder in *)
       eaddr
    end

  (* Construct code for an expression; return its value *)
  and expr env = 
    let evaluate_exprs env exprs =
      let (env', relements) = List.fold_left
                                (fun (env, values) e ->
                                  let (env', v) = expr env e in
                                  (env', v :: values))
                                (env, [])
                                exprs in
      (env', List.rev relements)
    in
    function
      A.IntLit i -> (env, L.const_int i32_t i)
    | A.BoolLit b -> 
       let llb = L.const_int i1_t (if b then 1 else 0) in
       debug (L.string_of_llvalue llb);
       (env, llb)
    | A.StringLit s -> (env, L.build_global_stringptr s "" env.builder)
    | A.FloatLit f -> (env, L.const_float dbl_t f)
    | A.TupleLit _ -> raise (Failure "tuples are currently not supported")
    (* let (env, elements) = evaluate_exprs env elist in *)
    (* (env, L.const_struct context (Array.of_list elements)) *)
    | A.ListLit elist ->
       if List.length elist == 0
       then raise (Failure "Empty lists are not supported")
       else
         let (env, elements) = evaluate_exprs env elist in
         debug ("elements = " ^ String.concat ", " (List.map L.string_of_llvalue elements));
         let etype = L.type_of (List.hd elements) in
         debug ("etype = " ^ L.string_of_lltype etype);
         (* let array = L.const_array etype (Array.of_list elements) in *)
         (* debug ("array = " ^ L.string_of_llvalue array); *)
         let num_elems = List.length elist in
         let ptr = L.build_array_malloc
                     etype
                     (L.const_int i32_t num_elems)
                     "" 
                     env.builder in
         (* debug ("ptr = " ^ L.string_of_llvalue ptr); *)
         (* let eptr = L.build_pointercast ptr  *)
         (*                                (L.pointer_type etype)  *)
         (*                                "" *)
         (*                                env.builder in *)
         ignore (List.fold_left 
                   (fun i elem ->
                     let ind = L.const_int i32_t i in
                     let eptr = L.build_gep ptr [|ind|] "" env.builder in
                     llstore elem eptr env.builder;
                     i+1
                   ) 0 elements);
         (env, ptr)
    | A.Noexpr -> (env, L.const_int i32_t 0)
    | A.Id id -> 
       (env, L.build_load (lookup env id) "" env.builder)
    | A.Binop (e1, op, e2) ->
       let (env, e1') = expr env e1 in
       let (env, e2') = expr env e2 in
       let exp_type = L.classify_type(L.type_of e1') in
       let exp_type2 = L.classify_type(L.type_of e2') in
       (match exp_type with 
          L.TypeKind.Double ->
          let e2_ = match exp_type2 with 
              L.TypeKind.Double -> e2' 
            | L.TypeKind.Integer -> (L.build_sitofp e2' dbl_t "" env.builder)
            | _ -> raise (Failure "Algebra only supports float and int.") in 
          (env,
           (match op with
              A.Add     -> L.build_fadd
            | A.Sub     -> L.build_fsub
            | A.Mult    -> L.build_fmul
            | A.Div     -> L.build_fdiv
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
            | A.Neq     -> L.build_fcmp L.Fcmp.One
            | A.Less    -> L.build_fcmp L.Fcmp.Ult
            | A.Leq     -> L.build_fcmp L.Fcmp.Ole
            | A.Greater -> L.build_fcmp L.Fcmp.Ogt
            | A.Geq     -> L.build_fcmp L.Fcmp.Oge
           ) e1' e2_ "tmp" env.builder)
        | _ ->
           (match exp_type2 with 
              L.TypeKind.Double -> 
              let e1_ = match exp_type with
                  L.TypeKind.Double -> e1'
                | L.TypeKind.Integer -> (L.build_sitofp e1' dbl_t "" env.builder)
                | _ -> raise (Failure "Algebra only supports float and int.") in 
              (env,
               (match op with
                  A.Add     -> L.build_fadd
                | A.Sub     -> L.build_fsub
                | A.Mult    -> L.build_fmul
                | A.Div     -> L.build_fdiv
                | A.And     -> L.build_and
                | A.Or      -> L.build_or
                | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
                | A.Neq     -> L.build_fcmp L.Fcmp.One
                | A.Less    -> L.build_fcmp L.Fcmp.Ult
                | A.Leq     -> L.build_fcmp L.Fcmp.Ole
                | A.Greater -> L.build_fcmp L.Fcmp.Ogt
                | A.Geq     -> L.build_fcmp L.Fcmp.Oge
               ) e1_ e2' "tmp" env.builder)
            | _ ->
               (env,
                (match op with
                   A.Add     -> L.build_add
                 | A.Sub     -> L.build_sub
                 | A.Mult    -> L.build_mul
                 | A.Div     -> L.build_sdiv
                 | A.And     -> L.build_and
                 | A.Or      -> L.build_or
                 | A.Equal   -> L.build_icmp L.Icmp.Eq
                 | A.Neq     -> L.build_icmp L.Icmp.Ne
                 | A.Less    -> L.build_icmp L.Icmp.Slt
                 | A.Leq     -> L.build_icmp L.Icmp.Sle
                 | A.Greater -> L.build_icmp L.Icmp.Sgt
                 | A.Geq     -> L.build_icmp L.Icmp.Sge
                ) e1' e2' "tmp" env.builder)
           )  
       )
    | A.Unop(op, e) ->
       let (env, e') = expr env e in
       (env,
        (match op with
           A.Neg     -> L.build_neg
         | A.Not     -> L.build_not) e' "tmp" env.builder)
    | A.Assign (s, e) -> 
       let (env, e') = expr env e in
       let addr = lookup env s in
       ignore (llstore e' addr env.builder);
       (env, e')

    | A.Call (A.NaiveId("printi"), [e]) ->
       let (env, v) = expr env e in
       (env, (L.build_call printf_func
                           [| int_format_str ; v |]
                           "printi" env.builder))

    | A.Call (A.NaiveId("printb"), [e]) ->
       let (env, v) = expr env e in
       (env, (L.build_call printf_func 
                           [| int_format_str ; v |]
                           "printb" env.builder))

    | A.Call (A.NaiveId("print"), [e]) -> 
       let (env, v) = expr env e in
       (env, L.build_call printf_func 
                          [| v |] "print" 
                          env.builder)
         
    | A.Call (A.NaiveId("printendl"), [e]) ->
       let (env, v) = expr env e in
       (env, L.build_call printf_func 
                          [| endl_str; v |] "printendl" 
                          env.builder)


    | A.Call (A.NaiveId("printf"), [e]) ->
       let (env, v) = expr env e in
       (env, L.build_call printf_func
                          [| float_format_str ; v |]
                          "printf" env.builder)

    | A.Call(A.NaiveId("concat"), [c1; c2]) ->
       let (env', v1) = expr env c1 in
       let (env'', v2) = expr env' c2 in
       (env'', L.build_call str_concat_func [| v1; v2 |] "" env''.builder)

    | A.Call(A.NaiveId("sizeof"), [e]) -> 
       let (env', v1) = expr env e in
       let i64_size = L.size_of (L.type_of v1 )in
       let i32_size = L.build_intcast i64_size i32_t "" env'.builder in
       (env', i32_size)
         
    | A.Call(A.NaiveId("parallel"), [f; pool; nthread]) ->
       let (env1, llf) = expr env f in
       let (env2, llpool) = expr env1 pool in
       let (env3, llnthread) = expr env2 nthread in

       let etype = L.element_type (L.type_of llpool) in
       let size = L.size_of etype in
       let i32_size = L.build_intcast size i32_t "" env3.builder in

       let fcast = L.build_bitcast llf subroutine_t "" env3.builder in
       let poolcast = L.build_bitcast llpool (L.pointer_type voidstar_t) "" env3.builder in
       let v_arr = [| fcast; poolcast; i32_size; llnthread |]  in
       (env3, L.build_call parallel_func v_arr "" env3.builder)

    | A.Call(A.NaiveId("free"), [e]) ->
       let (env1, e') = expr env e in
       let ept = L.build_bitcast e' voidstar_t "" env1.builder in
       (env1, L.build_free ept env1.builder)

    | A.Call(A.NaiveId("malloc"), [e]) ->
       let (env1, e') = expr env e in
       (env1, L.build_array_malloc i8_t e' "" env1.builder)

    | A.Call(A.NaiveId("mutex_create"), _) ->
       (env, L.build_call mutex_create_func [||] "" env.builder)

    | A.Call(A.NaiveId("mutex_lock"), [e]) ->
       let (env1, e') = expr env e in
       let mut = L.build_bitcast e' voidstar_t "" env1.builder in
       (env1, L.build_call mutex_lock_func [|mut|] "" env1.builder)

    | A.Call(A.NaiveId("mutex_unlock"), [e]) ->
       let (env1, e') = expr env e in
       let mut = L.build_bitcast e' voidstar_t "" env1.builder in
       (env1, L.build_call mutex_unlock_func [|mut|] "" env1.builder)

    | A.Call(A.NaiveId("mutex_destroy"), [e]) ->
       let (env1, e') = expr env e in
       let mut = L.build_bitcast e' voidstar_t "" env1.builder in
       (env1, L.build_call mutex_destroy_func [|mut|] "" env1.builder)

    | A.Call (f, act) ->
       let fptr = lookup env f in
       debug ("fptr = " ^ L.string_of_llvalue fptr);
       let fdef = L.build_load fptr "" env.builder in
       debug ("fdef = " ^ L.string_of_llvalue fdef);
       debug ("fdef type = " ^ L.string_of_lltype (L.type_of fdef));
       let (env, actuals) = List.fold_right
                              (fun e (env, values) ->
                                let (env', v) = expr env e in
                                (env', v :: values))
                              act
                              (env, [])
       in
       (env, L.build_call fdef (Array.of_list actuals) "" env.builder)
  in

  let rec init_of_type t = 
    match t with
      A.DataType(A.Int) -> L.const_int (ltype_of_typ t) 0
    | A.DataType(A.Bool) -> L.const_int (ltype_of_typ t) 0 
    | A.DataType(A.Float) -> L.const_float (ltype_of_typ t) 0.0 
    | A.DataType(A.String) -> L.const_string context "" 
    | A.List(_) -> L.const_null (ltype_of_typ t)
    | A.Struct(_, _) -> L.const_null (ltype_of_typ t)
    | A.UserType(_) -> let t' = S.resolve_user_type t user_types in
                       init_of_type t'
    | _ -> raise (Failure ("Global variable with unsupported type: " ^ (A.string_of_typ t)))
  in

  (* Declare each global variable; remember its value in a map *)
  let add_global env stmt = match stmt with
      A.Global(vd) -> 
      begin match vd with 
        A.Bind(t, id) ->
        let init = init_of_type t in
        debug ("init = " ^ (L.string_of_llvalue init));
        let var = L.define_global id init the_module in
        { env with externals = StringMap.add id var env.externals }
      | A.Binass(t, id, e) -> 
         let init = init_of_type t in
         debug ("init = " ^ (L.string_of_llvalue init));
         let var = L.define_global id init the_module in
         debug ("gvar type = " ^ (L.string_of_lltype (L.type_of var)));
         let (env', lval) = (expr env e) in
         ignore (llstore lval var env'.builder);
         { env' with externals = StringMap.add id var env'.externals }
      end
    | _ -> env
  in

  let global_env = 
    List.fold_left add_global
                   { externals = 
                       StringMap.mapi (
                           fun name (fval, _) ->
                           let ft = L.type_of fval in
                           let fvar = L.define_global (name ^ "_ptr") (L.const_pointer_null ft) the_module in
                           ignore (llstore fval fvar global_builder);
                           debug ("fvar type = " ^ (L.string_of_lltype (L.type_of fvar)));
                           fvar
                         ) function_decls;
                     locals = StringMap.empty;
                     builder = global_builder }
                   global_stmts
  in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let fbuilder = L.builder_at_end context (L.entry_block the_function) in

    (* Invoke "f env.builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal env f =
      match L.block_terminator (L.insertion_block env.builder) with
        Some _ -> ()
      | None -> ignore (f env.builder) in
    
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt env = function
        A.Block sl -> 
        let new_bb = L.append_block context "block" the_function in
        let cont_bb = L.append_block context "cont" the_function in
        let nenv = {
            locals = StringMap.empty;
            externals = 
              StringMap.merge (fun _ xo yo -> match xo,yo with
                                              | Some x, Some _ -> Some x 
                                              | None, yo -> yo
                                              | xo, None -> xo ) env.locals env.externals;
            builder = L.builder_at_end context new_bb
          } in

        let nenv' = (List.fold_left stmt nenv sl) in
        add_terminal env (L.build_br new_bb);
        add_terminal nenv' (L.build_br cont_bb);
        { env with builder = L.builder_at_end context cont_bb }

      | A.Expr e -> fst (expr env e)
      | A.Return e ->
         let (env', v) = expr env e in
         let ret = (match fdecl.A.typ with
                      A.DataType(A.Void) -> L.build_ret_void env'.builder
                    | _ -> L.build_ret v env'.builder) in
         debug ("ret = " ^ L.string_of_llvalue ret);
         env'
      | A.If (predicate, then_stmt, else_stmt) ->
         let (env', bool_val) = expr env predicate in
         let merge_bb = L.append_block context "merge" the_function in

         let then_bb = L.append_block context "then" the_function in
         add_terminal (stmt {env' with builder = (L.builder_at_end context then_bb)} then_stmt)
                      (L.build_br merge_bb);

         let else_bb = L.append_block context "else" the_function in
         add_terminal (stmt {env' with builder = (L.builder_at_end context else_bb)} else_stmt)
                      (L.build_br merge_bb);

         ignore (L.build_cond_br bool_val then_bb else_bb env.builder);
         {env with builder = L.builder_at_end context merge_bb}

      | A.While (predicate, body) ->
         let pred_bb = L.append_block context "while" the_function in
         ignore (L.build_br pred_bb env.builder);

         let pred_builder = L.builder_at_end context pred_bb in
         let (env', bool_val) = expr {env with builder = pred_builder} predicate in

         let body_bb = L.append_block context "while_body" the_function in
         add_terminal (stmt {env' with builder = (L.builder_at_end context body_bb)} body)
                      (L.build_br pred_bb);

         let merge_bb = L.append_block context "merge" the_function in
         ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
         {env with builder = L.builder_at_end context merge_bb}

      | A.For (e1, e2, e3, body) ->
         stmt env ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )

      | A.Local (vd) ->
         begin match vd with
           A.Bind(t, id) ->
           let local_var = 
             begin match (S.resolve_user_type t user_types) with
               A.Struct(_, _) -> 
               let struct_ptr_t = ltype_of_typ t in
               debug ("struct_ptr_t = " ^ L.string_of_lltype struct_ptr_t);
               let struct_t = L.element_type struct_ptr_t in
               debug ("struct_t = "  ^ L.string_of_lltype struct_t);
               let struct_ptr = L.build_alloca struct_ptr_t "" env.builder in
               let struct_val = L.build_malloc struct_t "" env.builder in
               ignore (llstore struct_val struct_ptr env.builder);
               struct_ptr
             | _ -> L.build_alloca (ltype_of_typ t) id env.builder
             end in
           {env with locals = StringMap.add id local_var env.locals}
         | A.Binass(t, id, e) -> 
            let local_var =  L.build_alloca (ltype_of_typ t) id env.builder in
            let (env, e') = expr env e in
            let env' = {env with locals = StringMap.add id local_var env.locals} in
            let p = (lookup env' (A.NaiveId id)) in
            ignore (llstore e' p env.builder);
            env'
         end
    in

    (* Build the code for each statement in the function *)
    let add_formal m (t, n) p =
      L.set_value_name n p;
      debug ("adding formal " ^ n ^ " of type " ^ L.string_of_lltype (ltype_of_typ t));
      let local = L.build_alloca (ltype_of_typ t) n fbuilder in
      ignore (L.build_store p local fbuilder);
      StringMap.add n local m
    in
    
    let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
                                  (Array.to_list (L.params the_function)) in

    let env = List.fold_left stmt 
                             { externals = global_env.externals;
                               locals    = formals;
                               builder   = fbuilder }
                             fdecl.A.body
    in

    (* Add a return if the last block falls off the end *)
    add_terminal env (match fdecl.A.typ with
                        A.DataType(A.Void) -> L.build_ret_void
                      | t -> L.build_ret (init_of_type t));
    ()
  in

  List.iter build_function_body functions;

  let llmem = L.MemoryBuffer.of_file "bindings.bc" in
  let llm = Llvm_bitreader.parse_bitcode context llmem in
  ignore (Llvm_linker.link_modules the_module llm Llvm_linker.Mode.PreserveSource);

  the_module
