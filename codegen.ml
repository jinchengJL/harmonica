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

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Harmonica"
  and i32_t  = L.i32_type    context
  and i8_t   = L.i8_type     context
  and i1_t   = L.i1_type     context
  and dbl_t  = L.double_type context
  and void_t = L.void_type   context
  and unknown_t = L.named_struct_type context "HarmonicaUnknownType"
  in

  let ltype_of_typ = function
      A.DataType(A.Int) -> i32_t
    | A.DataType(A.Bool) -> i1_t
    | A.DataType(A.Float) -> dbl_t
    | A.DataType(A.Void) -> void_t
    | _ -> i32_t
  in
  
  let vdecl_to_bind = function
      A.Bind(t, n) -> (t, n)
    | A.Binass(t, n, _) -> (t, n)
  in  

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var 
                   StringMap.empty
                   (List.fold_left
                      (fun acc -> function 
                          A.Global(vd) -> (vdecl_to_bind vd) :: acc
                        | _ -> acc)
                      []
                      globals) in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
	      Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
         StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
	      let local = L.build_alloca (ltype_of_typ t) n builder in
	      ignore (L.build_store p local builder);
	      StringMap.add n local m in

      let add_local m (t, n) =
	      let local_var = L.build_alloca (ltype_of_typ t) n builder
	      in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
                                    (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals [] in
      (* List.fold_left add_local formals fdecl.A.locals in *)

    (* Return the value for a variable or formal argument *)
    let rec lookup = function
        A.NaiveId(n) -> (try StringMap.find n local_vars
                         with Not_found -> StringMap.find n global_vars)
      | A.MemberId(id, _) -> 
         (* TODO: build_struct_gep *)
         lookup id
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
	      A.IntLit i -> L.const_int i32_t i
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.StringLit s -> L.build_global_stringptr s "" builder
      | A.FloatLit f -> L.const_float dbl_t f
      | A.TupleLit elist -> L.const_struct context (Array.of_list (List.map (expr builder) elist))
      | A.ListLit elist ->
         begin match elist with
           [] -> L.const_array unknown_t [||]
         | hd :: _  -> L.const_array (L.type_of (expr builder hd))
                                     (Array.of_list (List.map (expr builder) elist))
         end
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id id -> L.build_load (lookup id) "identifier" builder
      | A.Binop (e1, op, e2) ->
	       let e1' = expr builder e1
	       and e2' = expr builder e2 in
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
	       ) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
	       let e' = expr builder e in
	       (match op with
	          A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> 
         let e' = expr builder e in
	       ignore (L.build_store e' (lookup s) builder); e'

      (*| A.Call ("print", [e]) ->
         (match List.hd [e] with
          | A.StringLit s ->
             let head = expr builder (List.hd [e]) in
             let llvm_val = L.build_in_bounds_gep head [| L.const_int i32_t 0 |] "string_printf" builder in
             
             print_endline ";a.string print called";
             L.build_call printf_func [| llvm_val |] "string_printf" builder
          
          | _ -> print_endline ";_ print called"; L.build_call printf_func [| int_format_str ; (expr builder e) |] "abcd" builder
         )*)
      | A.Call (A.NaiveId("printi"), [e]) -> 
        L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder
      | A.Call (A.NaiveId("printb"), [e]) ->
	      L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder

      | A.Call (A.NaiveId("print"), [e]) -> let get_string = function A.StringLit s -> s | _ -> "" in
        let s_ptr = L.build_global_stringptr ((get_string e) ^ "\n") ".str" builder in
        L.build_call printf_func [| s_ptr |] "printf" builder

      | A.Call (A.NaiveId("printf"), [e]) ->
        L.build_call printf_func [| float_format_str ; (expr builder e) |]
        "printf" builder


      | A.Call (f, act) ->
         let fdef = lookup f in
         (* let (fdef, fdecl) = StringMap.find f function_decls in *)
         let actuals = List.rev (List.map (expr builder) (List.rev act)) in
         let result = A.string_of_id f ^ "_result" in
         L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
      	Some _ -> ()
      | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	      A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> 
         ignore (match fdecl.A.typ with
	                 A.DataType(A.Void) -> L.build_ret_void builder
	               | _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	       let merge_bb = L.append_block context "merge" the_function in

	       let then_bb = L.append_block context "then" the_function in
	       add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	                    (L.build_br merge_bb);

	       let else_bb = L.append_block context "else" the_function in
	       add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	                    (L.build_br merge_bb);

	       ignore (L.build_cond_br bool_val then_bb else_bb builder);
	       L.builder_at_end context merge_bb
                    
      | A.While (predicate, body) ->
	       let pred_bb = L.append_block context "while" the_function in
	       ignore (L.build_br pred_bb builder);

	       let body_bb = L.append_block context "while_body" the_function in
	       add_terminal (stmt (L.builder_at_end context body_bb) body)
	                    (L.build_br pred_bb);

	       let pred_builder = L.builder_at_end context pred_bb in
	       let bool_val = expr pred_builder predicate in

	       let merge_bb = L.append_block context "merge" the_function in
	       ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
	       L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) ->
         stmt builder
	            ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )

      (* TODO: fill this out correctly, right now its just a placeholder *)
      (* NOTE: this may require significant change to codegen structure, need to add env similar to semant *)
      | A.Local (vd) ->
         begin match vd with
           A.Bind(_, _) -> builder
         | A.Binass(_, _, _) -> builder
         end
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
                            A.DataType(A.Void) -> L.build_ret_void
                          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
