open Cabs

let stringl lst = String.concat "" lst
let string_commas fct lst = String.concat ", " (List.map fct lst)

let rec string_of_type_spec = function
    Tvoid -> "void"
  | Tchar -> "char"
  | Tbool -> "bool"
  | Tshort -> "short"
  | Tint -> "int"
  | Tlong -> "long"
  | Tint64 -> "int64_t"
  | Tfloat -> "float"
  | Tdouble -> "double"
  | Tsigned -> "signed"
  | Tunsigned -> "unsigned"
  | Tnamed (name) -> name
  | Tstruct (n, None, _) -> stringl ["struct ";n]
  | Tstruct (n, Some flds, extraAttrs) ->
     (string_of_struct_name_attr "struct" n extraAttrs) ^
       (string_of_fields flds)
  | Tunion (n, None, _) -> stringl ["union ";n]
  | Tunion (n, Some flds, extraAttrs) ->
     (string_of_struct_name_attr "union" n extraAttrs) ^
       (string_of_fields flds)
  | Tenum (n, None, _) -> stringl ["enum ";n]
  | Tenum (n, Some enum_items, extraAttrs) ->
     (string_of_struct_name_attr "enum" n extraAttrs) ^
       (string_of_enum_items enum_items)
  | _ -> "string_of_type_spec: not yet implemented"

and string_of_struct_name_attr keyword name extraAttrs =
  if extraAttrs = []
  then stringl [keyword; " "; name]
  else stringl [keyword; " "; string_of_attrs extraAttrs; name]
               
and string_of_decl (n: string) = function
    JUSTBASE -> if n <> "___missing_field_name" then n
                else "/*missing field name*/"
  | PARENTYPE (al1, d, al2) ->
     "(" ^ stringl [string_of_attrs al1;
                    string_of_decl n d;
                    string_of_attrs] ^ ")"
  | PTR (al, d) ->
      "* " ^ string_of_attrs al ^ " " ^ string_of_decl n d
  | ARRAY (d, al, e) ->
     let estr = if e = NOTHING then "" else string_of_expression e in
     string_of_decl n d ^ "[" ^ string_of_attrs al ^ estr ^ "]"
  | PROTO(d, args, isva) ->
     string_of_decl n d ^ "(" ^ string_of_params args isva ^ ")"

and string_of_fields (flds : field_group list) =
 " { " ^ stringl List.map (fun fld -> string_of_field_group ^ "; ") flds ^ " } "

and string_of_enum_items items =
  let string_of_eitem (id, exp, _) =
    id ^ if exp = NOTHING then ""
	       else " = " ^ string_of_expression exp
  in " { " ^ String.concat ", " (List.map string_of_eitem items) ^ " } "

and string_of_spec_elem = function
    SpecTypedef -> "typedef"
  | SpecInline -> "inline"
  | SpecStorage sto ->
     (match sto with
        NO_STORAGE -> "/*no storage*/"
      | AUTO -> "auto"
      | STATIC -> "static"
      | EXTERN -> "extern"
      | REGISTER -> "register")
  | SpecCV cv ->
     (match cv with
        CV_CONST -> "const"
      | CV_VOLATILE -> "volatile"
      | CV_RESTRICT -> "restrict")
  | SpecAttr al -> string_of_attr al
  | SpecType bt -> string_of_type_spec bt
  | SpecPattern name -> "string_of_spec_elem: not yet implemented (and I've no idea what this is)"

and string_of_onlytype (specs, dt) =
  string_of_specifiers specs ^ " " ^ string_of_decl "" dt
             
and string_of_name ((n, decl, attrs, _) : name) =
  string_of_decl n decl ^ " " ^ string_of_attrs attrs

and string_of_init_name ((n, i) : init_name) =
  string_of_name n ^ 
    if i = NO_INIT
    then ""
    else " = " ^ string_of_init_expression i
            
and string_of_name_group (specs, names) =
  string_of_specifiers specs ^ " " ^ (string_commas string_of_name names)
    
and string_of_field_group (specs, fields) =
  string_of_specifiers specs ^ " " ^ (string_commas string_of_field fields)
               
and string_of_single_name (specs, name) =
  string_of_specifiers specs ^ " " ^ string_of_name name

and string_of_params (pars : single_name list) (ell : bool) =
  string_commas string_of_single_name pars ^ 
    if ell then (if pars = [] then "..." else ",...") else ""

and string_of_def = function
   FUNDEF (sname, body, _, _) -> string_of_single_name sname ^ " " ^ string_of_block body
 | DECDEF (init_name_group, loc) -> ""
 | TYPEDEF (name_group, loc) -> ""
 | ONLYTYPEDEF (specifier, loc) -> ""
 | DIRECTIVE (c, loc) -> ""
 | LINKAGE (name, loc, deflist) -> ""
 | _ -> "string_of_def: not yet implemented"

and string_of_file (name, defs) = 
  let namestr = "/* file: " ^ name ^ " */\n" in
  namestr ^ (String.concat "\n" (List.map string_of_def defs))

