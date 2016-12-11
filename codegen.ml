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

let translate (global_stmts, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Harmonica"
  and i32_t  = L.i32_type    context
  and i8_t   = L.i8_type     context
  and i1_t   = L.i1_type     context
  and dbl_t  = L.double_type context
  and void_t = L.void_type   context
  and string_t = L.pointer_type (L.i8_type context)
  in

  (* user-defined types *)
  let user_types = List.fold_left
                     (fun map -> function
                         A.Typedef(t, s) -> StringMap.add s t map
                       | _ -> map)
                     StringMap.empty
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
    | A.List(t) -> L.array_type (ltype_of_typ t) 3
    (* TODO: channels *)
    | A.Struct(name, blist) ->
      (try StringMap.find name !typ_cache
       with Not_found -> 
       let struct_t = L.named_struct_type context name in
       L.struct_set_body struct_t (Array.of_list (List.map ltype_of_typ (List.map fst blist))) false;
       typ_cache := StringMap.add name struct_t !typ_cache;
       struct_t)

    | A.UserType(_) as t -> let t' = S.resolve_user_type t user_types in
                            ltype_of_typ t'
    | A.FuncType(tlist) ->
       let reversed = List.map ltype_of_typ (List.rev tlist) in
       let return_t = List.hd reversed in
       let params_t = Array.of_list (List.rev (List.tl reversed)) in
       L.function_type return_t params_t
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

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
	      Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in


  let (main_function, _) = StringMap.find "main" function_decls in
  (*let block_before_main = L.insert_block context "__global__" (L.entry_block main_function) in*)
  let global_builder = L.builder_at_end context (L.entry_block main_function) in

  let int_format_str = L.build_global_stringptr "%d\n" "fmt" global_builder in
  let float_format_str = L.build_global_stringptr "%f\n" "fmt" global_builder in

  (* Return the address of a variable or formal argument *)
  let rec lookup env x = 
    (* prerr_endline ("looking up " ^ A.string_of_id x); *)
    begin match x with
      A.NaiveId(n) -> 
      (try StringMap.find n env.locals
        with Not_found -> 
          (try StringMap.find n env.externals
          with Not_found -> raise (Failure ("undeclared variable " ^ n))))
    | A.MemberId(id, field_name) -> 
        let rec find_index_of field_name list = 
          match list with
            [] -> raise (Failure ("undeclared field " ^ field_name))
          | hd :: tl -> if field_name = (snd hd) 
                        then 0 
                        else 1 + find_index_of field_name tl
        in
        let container_addr = lookup env id in
        let container = L.build_load container_addr "" env.builder in 
        let container_tname_opt = L.struct_name (L.type_of container) in
        (match container_tname_opt with
          None -> raise (Failure ("expected struct, found tuple: " ^ A.string_of_id id))
        | Some(container_tname) ->
            let fields = StringMap.find container_tname struct_map in
            let idx = find_index_of field_name fields in
            let addr = L.build_struct_gep container_addr idx "" env.builder in
            addr)
    | A.IndexId(id, e) ->
        let container_addr = lookup env id in
        let container = L.build_load container_addr "" env.builder in
        let index = snd (expr env e) in 
        let addr = L.build_gep container_addr [| index |] "tmp" env.builder in
        let oink = L.build_pointercast addr (L.pointer_type (L.element_type (L.type_of container))) "" env.builder in
        oink
    end

  (* Construct code for an expression; return its value *)
    and expr env = 
    let evaluate_exprs env exprs = List.fold_left 
                                      (fun (env, values) e ->
                                        let (env', v) = expr env e in
                                        (env', v :: values))
                                      (env, [])
                                      exprs
    in
    function
      A.IntLit i -> (env, L.const_int i32_t i)
    | A.BoolLit b -> (env, L.const_int i1_t (if b then 1 else 0))
    | A.StringLit s -> (env, L.build_global_stringptr s "" env.builder)
    | A.FloatLit f -> (env, L.const_float dbl_t f)
    | A.TupleLit elist -> 
        let (env, elements) = evaluate_exprs env elist in
        (env, L.const_struct context (Array.of_list elements))
    | A.ListLit elist ->
        if List.length elist == 0 
        then raise (Failure "Empty lists are not supported")
        else
          let (env, elements) = evaluate_exprs env elist in
          (env, L.const_array (L.type_of (List.hd elements))
                              (Array.of_list (List.rev elements)))
    | A.Noexpr -> (env, L.const_int i32_t 0)
    | A.Id id -> 
        (env, L.build_load (lookup env id) "identifier" env.builder)
    | A.Binop (e1, op, e2) ->
        let (env, e1') = expr env e1 in
        let (env, e2') = expr env e2 in
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
    | A.Unop(op, e) ->
        let (env, e') = expr env e in
        (env,
        (match op with
            A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" env.builder)
    | A.Assign (s, e) -> 
        let (env, e') = expr env e in
        let addr = lookup env s in

(*          prerr_endline (L.string_of_llvalue addr);
        prerr_endline (L.string_of_lltype (L.type_of addr));
        prerr_endline (L.string_of_lltype (L.type_of e')); *)
        
        ignore (L.build_store e' addr env.builder);

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
        let s_ptr = snd (expr env e) in
        (env, L.build_call printf_func 
                          [| s_ptr |] "print" 
                          env.builder)

    | A.Call (A.NaiveId("printf"), [e]) ->
        let (env, v) = expr env e in
        (env, L.build_call printf_func
                          [| float_format_str ; v |]
                          "printf" env.builder)

    | A.Call (f, act) ->
        let fdef = lookup env f in
        (* let (fdef, fdecl) = StringMap.find f function_decls in *)
        let (env, actuals) = List.fold_left 
                              (fun (env, values) e ->
                                let (env', v) = expr env e in
                                (env', v :: values))
                              (env, [])
                              act in
        let result = A.string_of_id f ^ "_result" in
        (env, L.build_call fdef (Array.of_list actuals) result env.builder)
  in

  let rec init_of_type t = 
    match t with
      A.DataType(A.Int) -> L.const_int (ltype_of_typ t) 0 
    | A.DataType(A.Bool) -> L.const_int (ltype_of_typ t) 0 
    | A.DataType(A.Float) -> L.const_float (ltype_of_typ t) 0.0 
    | A.DataType(A.String) -> L.const_string context "" 
    | A.List(t') -> L.const_array (ltype_of_typ t') [||]
    | A.Struct(_, blist) -> 
       let tlist = List.map fst blist in 
       L.const_named_struct (ltype_of_typ t) 
                            (Array.of_list (List.map init_of_type tlist))
    | A.UserType(_) as t -> let t' = S.resolve_user_type t user_types in
                            init_of_type t'
    | _ -> raise (Failure "Please see documentation. Grammar error. ")
  in

  (* Declare each global variable; remember its value in a map *)
  let global_env = 
    List.fold_left 
    (fun env stmt -> 
    begin match stmt with
        A.Global(vd) -> 
          begin match vd with 
            A.Bind(t, id) -> 
              let init = init_of_type t in
              let var = L.define_global id init the_module in
              { env with externals = StringMap.add id var env.externals }
          | A.Binass(_, id, e) -> 
              let (env', init) = (expr env e) in
              let var = L.define_global id init the_module in
              prerr_endline (L.string_of_lltype (L.type_of var));
              (* ignore(L.build_store value var global_builder); *)
              { env' with externals = StringMap.add id var env'.externals }
          end
      | _ -> env
     end)
    { externals = StringMap.empty; locals = StringMap.empty; builder = global_builder }
    global_stmts
  in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

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

        let nenv = {
          locals = StringMap.empty;
          externals = 
            StringMap.merge (fun _ xo yo -> match xo,yo with
                | Some x, Some _ -> Some x 
                | None, yo -> yo
                | xo, None -> xo ) env.locals env.externals;
          builder = env.builder
        } in
        
        ignore(List.fold_left stmt nenv sl); 
        env

      | A.Expr e -> fst (expr env e)
      | A.Return e ->
         let (env, v) = expr env e in
         ignore (match fdecl.A.typ with
	                 A.DataType(A.Void) -> L.build_ret_void builder
	               | _ -> L.build_ret v env.builder);
         env
      | A.If (predicate, then_stmt, else_stmt) ->
         let (env', bool_val) = expr env predicate in
	       let merge_bb = L.append_block context "merge" the_function in

	       let then_bb = L.append_block context "then" the_function in
	       add_terminal (stmt {env' with builder = (L.builder_at_end context then_bb)} then_stmt)
	                    (L.build_br merge_bb);

	       let else_bb = L.append_block context "else" the_function in
	       add_terminal (stmt {env' with builder = (L.builder_at_end context else_bb)} else_stmt)
	                    (L.build_br merge_bb);

	       ignore (L.build_cond_br bool_val then_bb else_bb builder);
	       {env with builder = L.builder_at_end context merge_bb}

      | A.While (predicate, body) ->
	       let pred_bb = L.append_block context "while" the_function in
	       ignore (L.build_br pred_bb builder);

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
            let local_var = L.build_alloca (ltype_of_typ t) id builder in
            {env with locals = StringMap.add id local_var env.locals} 
         | A.Binass(t, id, e) -> 
            
            let local_var = L.build_alloca (ltype_of_typ t) id builder in
            let (env, e') = expr env e in
            let env' = {env with locals = StringMap.add id local_var env.locals} in

            let e_type = L.type_of e' in 
            let p = (lookup env' (A.NaiveId id)) in
            match (L.classify_type e_type) with 
              L.TypeKind.Array -> (
                  let _ = L.array_length e_type in
                  ignore (L.build_store e' p env.builder);
                  prerr_endline (L.string_of_llvalue e');
                  prerr_endline (L.string_of_llvalue p);
                  (* TODO: array can't bulk load *)
                  env'
                )
              | _ -> (
                prerr_endline (L.string_of_llvalue p);
                ignore (L.build_store e' p env.builder);
                env' 
              )
         end
    in

    (* Build the code for each statement in the function *)
    let add_formal m (t, n) p =
      L.set_value_name n p;
	    let local = L.build_alloca (ltype_of_typ t) n builder in
	    ignore (L.build_store p local builder);
	    StringMap.add n local m
    in
    
    let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
                                  (Array.to_list (L.params the_function)) in
  
    let env = List.fold_left stmt 
                             { externals = global_env.externals;
                               locals    = formals;
                               builder   = builder }
                             fdecl.A.body 
    in

    (* Add a return if the last block falls off the end *)
    add_terminal env (match fdecl.A.typ with
                        A.DataType(A.Void) -> L.build_ret_void
                      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
