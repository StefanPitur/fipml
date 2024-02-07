open Core
open Parsing


let pprint_constructors_env_entry 
    (ppf : Format.formatter)
    (constructor_env_entry : Type_defns_env.constructor_env_entry)
  : unit = 

  let Type_defns_env.ConstructorEnvEntry(constructor_type, constructor_name, constructor_args) = constructor_env_entry in
  Fmt.pf ppf "Constructor Type - %s@." (Ast.Ast_types.Type_name.to_string constructor_type);
  Fmt.pf ppf "Constructor Name - %s@." (Ast.Ast_types.Constructor_name.to_string constructor_name);
  List.iter 
    constructor_args
    ~f: (fun constructor_arg -> Ast.Pprint_ast_types.pprint_type_expr ppf ~indent:"    " constructor_arg);
  Fmt.pf ppf "----------------------------\n@."

let pprint_types_env_entry
    (ppf : Format.formatter)
    (types_env_entry : Ast.Ast_types.Type_name.t)
  : unit = 
  Fmt.pf ppf "Type Name - %s@." (Ast.Ast_types.Type_name.to_string types_env_entry);
  Fmt.pf ppf "----------------------------\n@."


let typecheck_program (Parser_ast.TProg (type_defns, _, _)) :  (Typed_ast.program Or_error.t) = 
  let open Result in
  Typecheck_type_defns.typecheck_type_defns [] [] [] type_defns
  >>= fun (types_env, constructors_env, typed_ast_type_defns) -> 
    List.iter types_env ~f:(fun types_env_entry -> pprint_types_env_entry Fmt.stdout types_env_entry);
    Fmt.pf Fmt.stdout "\n\n";
    List.iter constructors_env ~f:(fun constructors_env_entry -> pprint_constructors_env_entry Fmt.stdout constructors_env_entry);
    Fmt.pf Fmt.stdout "><><><><><><><><><><><><><><><><><><><><><><\n\n";
    Ok (Typed_ast.TProg(typed_ast_type_defns, [], None))
