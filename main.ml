let () = 
  (* Taking the string given as a parameter or the program *)
  let lb = Lexing.from_channel stdin in
  let result = Parser.program Scanner.token lb in
  print_string (Ast.string_of_program result); print_newline()
