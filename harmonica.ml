(* Top-level of the Harmonica compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

type action = Ast | Print | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
                 List.assoc Sys.argv.(1) [ ("-a", Ast);      (* Print the AST only *)
                                           ("-p", Print);  (* Generate and print C *)
                                           ("-c", Compile) ] (* Generate and compile C *)
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  Semant.check ast;
  match action with
    Ast -> print_string (Ast.string_of_program ast)
  | Print -> Cprint.printFile stdout (Ccodegen.translate ast)
  | Compile -> let cabs = Ccodegen.translate ast in
               ignore cabs
