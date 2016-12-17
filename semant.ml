(* Semantic checking for the Harmonica compiler *)

open Ast

module StringMap = Map.Make(String)

type environment = {
    externals: typ StringMap.t;
    locals: typ StringMap.t;
  }

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong. *)

let rec resolve_user_type usert utypes =
  (match usert with
     UserType(s) -> (try resolve_user_type (StringMap.find s utypes) utypes
                     with Not_found -> raise (Failure ("undefined type " ^ s)))
   | _ -> usert)
    
let check (global_stmts, functions) =

  (* User-defined types *)
  let user_types = List.fold_left
                     (fun map -> function
                         Typedef(t, s) -> StringMap.add s t map
                       | _ -> map)
                     StringMap.empty
                     global_stmts
  in

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
	n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (DataType(Void), n) -> raise (Failure (exceptf n))
    | _ -> ()
  in


  (* Structural type equality *)
  let rec typ_equal t1 t2 = 
    (match (t1, t2) with
       (DataType(p1), DataType(p2)) -> if p1 = Unknown || p2 = Unknown
                                       then true
                                       else p1 = p2
     | (Tuple(tlist1), Tuple(tlist2)) -> 
        List.for_all2 typ_equal tlist1 tlist2
     | (List(t1'), List(t2')) -> typ_equal t1' t2'
     | (Channel(t1'), Channel(t2')) -> typ_equal t1' t2'
     | (Struct(name1, _), Struct(name2, _)) -> name1 = name2 (* TODO: ok? *)
     | (UserType(_), UserType(_)) -> 
        typ_equal (resolve_user_type t1 user_types) (resolve_user_type t2 user_types)
     | (FuncType(tlist1), FuncType(tlist2)) -> 
        List.for_all2 typ_equal tlist1 tlist2
     | _ -> false
    ) in

  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if typ_equal lvaluet rvaluet then lvaluet else raise err
  in

  (**** Checking Functions Definitions ****)
  let builtins = ["print"; "printb"; "printf"; "printi"; "printendl"; "concat"] in
  let builtin_duplicates = List.filter (fun fname -> List.mem fname builtins)
                                       (List.map (fun fd -> fd.fname) functions) in
  if List.length builtin_duplicates > 0
  then raise (Failure ("function " ^ 
                         (String.concat ", " builtin_duplicates) ^ 
                           " may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Global variable table *)
  let builtins = List.fold_left 
                   (fun map (name, t) -> StringMap.add name t map) 
                   StringMap.empty
                   [("print", FuncType([DataType(Void); DataType(String)]));
                    ("printb", FuncType([DataType(Void); DataType(Bool)]));
                    ("printf", FuncType([DataType(Void); DataType(Float)]));
                    ("printi", FuncType([DataType(Void); DataType(Int)]));
                    ("printendl", FuncType([DataType(Void); DataType(String)]));
                    ("concat", FuncType([DataType(String); DataType(String); DataType(String)]))
                    (* TODO: built-in multi argument functions, like concat*)
                    ]
  in

  let get_functype fdecl = FuncType(fdecl.typ :: (List.map fst fdecl.formals)) in
  let global_funcs = List.fold_left
                       (fun map fd -> StringMap.add fd.fname 
                                                    (get_functype fd)
                                                    map)
                       builtins
                       functions
  in

  (* Ensure "main" is defined *)
  ignore (try List.find (fun f -> f.fname = "main") functions
          with Not_found -> raise (Failure ("main function undefined")));

  (* NOTE: inner-scope variable overrides outer-scope variable with same name *)
  let rec type_of_identifier env = function
      NaiveId(s) -> 
      (try StringMap.find s (env.locals)
       with Not_found -> (
         try StringMap.find s (env.externals)
         with Not_found -> raise (Failure ("undeclared identifier " ^ s))))
    | MemberId(id, n) -> 
       let container_type = resolve_user_type (type_of_identifier env id) user_types in
       (match container_type with
          Struct(_, blist) -> 
          let (t, _) = List.find (fun (_, n') -> n' = n) blist in t
        | _ -> raise (Failure (string_of_id id  ^ " is not a struct type")))
    | IndexId(id, e) ->
        let container_type = resolve_user_type (type_of_identifier env id) user_types in
      (match container_type with
         List(t) -> (match expr env e with DataType(Int) -> t | _ -> raise (Failure "WTF. Must be int."))
       | _ -> raise (Failure "WTF. Must be list.")
      )

  (* Return the type of an expression or throw an exception *)
  and expr env = function
	    IntLit _ -> DataType(Int)
    | BoolLit _ -> DataType(Bool)
    | StringLit _ -> DataType(String)
    | FloatLit _ -> DataType(Float)
    | TupleLit elist -> Tuple (List.map (expr env) elist)
    | ListLit elist as e ->
       let tlist = List.map (expr env) elist in
       if (List.length tlist) = 0
       then List(DataType(Unknown))
       else
         let canon = List.hd tlist in
         if List.for_all (fun t -> t = canon) tlist
         then List(canon)
         else raise (Failure ("inconsistent types in list literal " 
                              ^ string_of_expr e))
    | Id s -> type_of_identifier env s
    | Binop(e1, op, e2) as e -> 
       let t1 = expr env e1 and t2 = expr env e2 in
	     (match op with
          Add | Sub | Mult | Div  -> 
          (match t1 with 
              DataType(Float) ->  DataType(Float) 
            | _ ->
                (match t2 with 
                DataType(Float) -> DataType(Float)
                | _ -> DataType(Int)
                )

          )
	        | Equal | Neq when t1 = t2 -> DataType(Bool)
	        | Less | Leq | Greater | Geq 
               when (t1 = t2) && (t1 = DataType(Int) || t1 = DataType(Float)) 
            -> DataType(Bool)
	        | And | Or when t1 = DataType(Bool) && t2 = DataType(Bool) -> DataType(Bool)
          | _ -> raise (Failure ("illegal binary operator " ^
                                   string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                                     string_of_typ t2 ^ " in " ^ string_of_expr e))
       )
    | Unop(op, e) as ex -> 
       let t = expr env e in
	     (match op with

	        Neg ->
          (match t with
              DataType(Float) -> DataType(Float)
            | DataType(Int) -> DataType(Int)
            |  _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		                         string_of_typ t ^ " in " ^ string_of_expr ex))
          )
	      | Not when t = DataType(Bool) -> DataType(Bool)
        | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		                         string_of_typ t ^ " in " ^ string_of_expr ex)))
       
    | Noexpr -> DataType(Void)
    | Assign(var, e) as ex -> let lt = type_of_identifier env var
                              and rt = expr env e in
                              check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				                                                     " = " ^ string_of_typ rt ^ " in " ^ 
				                                                       string_of_expr ex))
    | Call(fname, actuals) as call -> 
       let ftype = type_of_identifier env fname in
       (match ftype with
          FuncType(tlist) ->
          let formals = List.tl tlist in
          let ret = List.hd tlist in
          if List.length actuals != List.length formals then
            raise (Failure ("expecting " ^ 
                              string_of_int (List.length formals) ^ 
                                " arguments in " ^ string_of_expr call))
          else
            List.iter2 
              (fun ft e -> 
                let et = expr env e in
                ignore (check_assign ft et (Failure ("illegal actual argument found " ^ string_of_typ et ^ " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
              formals actuals;
          ret
        | _ -> raise (Failure (string_of_id fname ^ " is not a function"))
       )
  in

  (*** Checking Global Variables ***)
  let add_bind env t name = 
    check_not_void (fun n -> "illegal void variable " ^ n) (t, name);
    if StringMap.mem name env.locals then
      raise (Failure ("redefinition of " ^ name))
    else {
        externals = env.externals;
        locals = StringMap.add name t env.locals
      }
  in

  let add_vdecl env = function
      Bind(t, name) -> add_bind env t name
    | Binass(t, name, e) -> 
       let rtype = expr env e in
       ignore (check_assign t rtype 
                            (Failure ("illegal assignment " ^ string_of_typ t ^
				                                " = " ^ string_of_typ rtype ^ " in " ^ 
				                                  string_of_expr e)));
       add_bind env t name
  in

  let global_env = List.fold_left
                     (fun env -> function
                         Global(vd) -> add_vdecl env vd
                       | _ -> env)
                     { externals = StringMap.empty;
                       locals = global_funcs } 
                     global_stmts
  in

  let global_env = { externals = global_env.locals;
                     locals = StringMap.empty } in

  (*** Checking Function Contents ***)
  let check_function fenv func =
    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    (* Local variables and formals *)
    let fenv = List.fold_left 
                (fun e (t, name) -> add_bind e t name) fenv func.formals in

    let check_bool_expr env e = 
      let t = expr env e in
      if typ_equal t (DataType Bool)
      then ()
      else raise (Failure ("expected boolean expression in " ^ string_of_expr e))
    in

    (* Verify a statement or throw an exception, returns updated environment *)
    let rec stmt senv = function
	      Block sl -> 
        let rec check_block benv = function
            [Return _ as s] -> stmt benv s
          | Return _ :: _ -> raise (Failure "nothing may follow a return")
          | s :: ss -> check_block (stmt benv s) ss
          | [] -> benv
        in
        (* New symbol table for new scope *)
        let benv = {
            locals = StringMap.empty; 
            externals = 
              StringMap.merge (fun _ xo yo -> match xo,yo with
                | Some x, Some _ -> Some x 
                | None, yo -> yo
                | xo, None -> xo ) senv.locals senv.externals } in
        ignore (check_block benv sl);
        senv
      | Expr e -> ignore (expr senv e); senv
      | Return e -> let t = expr senv e in 
                    if typ_equal t func.typ then senv else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
      | If(p, b1, b2) -> check_bool_expr senv p; 
                         ignore (stmt senv b1);
                         ignore (stmt senv b2);
                         senv
      | For(e1, e2, e3, st) -> 
         ignore (expr senv e1);
         check_bool_expr senv e2;
         ignore (expr senv e3); ignore (stmt senv st);
         senv
      | While(p, s) -> check_bool_expr senv p; ignore (stmt senv s); senv
      | Local(vd) -> add_vdecl senv vd
    in
    ignore (stmt fenv (Block func.body))

  in
  List.iter (check_function global_env) functions
