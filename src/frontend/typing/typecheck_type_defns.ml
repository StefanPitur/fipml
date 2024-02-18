open Ast
open Core
open Parsing
open Type_envs


let rec typecheck_type_constructor_arg
    (types_env : types_env)
    (constructor_arg : Ast_types.type_expr)
  : unit Or_error.t =
  
  match constructor_arg with
  | Ast_types.TECustom (loc, custom_arg_type) -> 
      assert_custom_type_in_types_env loc custom_arg_type types_env
  | Ast_types.TEOption (_, option_arg_type) ->
      typecheck_type_constructor_arg types_env option_arg_type
  | Ast_types.TEArrow (_, input_type, output_type) ->
      let open Result in
      typecheck_type_constructor_arg types_env input_type
      >>= fun () -> typecheck_type_constructor_arg types_env output_type
  | _ -> Ok ()


let rec typecheck_type_constructor_args
    (types_env : types_env)
    (constructor_args : Ast_types.type_expr list)
  : unit Or_error.t =
  
  match constructor_args with
  | [] -> Ok ()
  | constructor_arg :: constructor_args ->
      let open Result in
      typecheck_type_constructor_arg types_env constructor_arg
      >>= fun () -> typecheck_type_constructor_args types_env constructor_args


let typecheck_type_constructor
    (types_env : types_env)
    (constructors_env : constructors_env)
    (constructor_type : Ast_types.Type_name.t)
    ((TTypeConstructor(loc, constructor_name, constructor_args)) : Parser_ast.type_constructor)
  : (constructors_env * Typed_ast.type_constructor) Or_error.t =
  
  let open Result in
  assert_constructor_not_in_constructors_env loc constructor_name constructors_env
  >>= fun () -> typecheck_type_constructor_args types_env constructor_args
  >>= fun () ->
        let constructor_env_entry = ConstructorEnvEntry(constructor_type, constructor_name, constructor_args) in
        let typed_ast_type_constructor = Typed_ast.TTypeConstructor(loc, constructor_name, constructor_args) in
        Ok (constructor_env_entry :: constructors_env, typed_ast_type_constructor)


let rec typecheck_type_constructors 
    (types_env : types_env)
    (constructors_env : constructors_env) 
    (typed_ast_type_constructors : Typed_ast.type_constructor list)
    (constructors_type : Ast_types.Type_name.t)
    (type_constructors : Parser_ast.type_constructor list) 
  : (constructors_env * Typed_ast.type_constructor list) Or_error.t =

  match type_constructors with
  | [] -> Ok (constructors_env, typed_ast_type_constructors)
  | type_constructor :: type_constructors -> 
      let open Result in
      typecheck_type_constructor types_env constructors_env constructors_type type_constructor
      >>= fun (constructors_env, typed_ast_type_constructor) -> 
        typecheck_type_constructors types_env constructors_env (typed_ast_type_constructor :: typed_ast_type_constructors) constructors_type type_constructors


let typecheck_type_defn
    (types_env : types_env)
    (constructors_env : constructors_env)
    (TType(loc, type_name, type_constructors) : Parsing.Parser_ast.type_defn)
  : (types_env * constructors_env * Typed_ast.type_defn) Or_error.t =

  let extended_types_env = type_name :: types_env in
  let open Result in
  assert_custom_type_not_in_types_env loc type_name types_env
  >>= fun () -> typecheck_type_constructors extended_types_env constructors_env [] type_name type_constructors
  >>= fun (constructors_env, typed_ast_type_constructors) -> 
    Ok (
      extended_types_env, 
      constructors_env, 
      Typed_ast.TType(loc, type_name, typed_ast_type_constructors)
    )


let rec typecheck_type_defns_wrapper 
    (types_env : types_env)
    (constructors_env : constructors_env)
    (typed_ast_type_defns : Typed_ast.type_defn list)
    (type_defns : Parser_ast.type_defn list)
  : (types_env * constructors_env * Typed_ast.type_defn list) Or_error.t =

  match type_defns with
  | [] -> Ok (types_env, constructors_env, typed_ast_type_defns)
  | type_defn :: type_defns ->
    let open Result in
      typecheck_type_defn types_env constructors_env type_defn
      >>= fun (types_env, constructors_env, typed_ast_type_defn) -> 
        typecheck_type_defns_wrapper types_env constructors_env (typed_ast_type_defn :: typed_ast_type_defns) type_defns

let typecheck_type_defns
    (type_defns : Parser_ast.type_defn list)
  : (types_env * constructors_env * Typed_ast.type_defn list) Or_error.t =
  typecheck_type_defns_wrapper [] [] [] type_defns

let rec pprint_types_env (ppf : Format.formatter) (types_env : types_env) : unit =
  match types_env with
  | [] -> ()
  | _type :: types_env ->
    Fmt.pf ppf "%s@." (Ast_types.Type_name.to_string _type);
    pprint_types_env ppf types_env

let rec pprint_constructors_env (ppf : Format.formatter) (constructors_env : constructors_env) : unit =
  let mock_loc : Lexing.position = {
    pos_fname = "mock";
    pos_lnum = 0;
    pos_bol = 0;
    pos_cnum = 0;
  } in
  let indent = "    " in
  match constructors_env with
  | [] -> ()
  | (ConstructorEnvEntry(type_name, constructor_name, constructor_params)) :: constructors_env ->
    Fmt.pf ppf "Constructor Type : %s@." (Ast_types.Type_name.to_string type_name);
    Pprint_parser_ast.pprint_type_constructor ppf ~indent:indent (Parser_ast.TTypeConstructor (mock_loc, constructor_name, constructor_params));
    pprint_constructors_env ppf constructors_env
