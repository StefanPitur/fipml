open Parsing
open Ast
open Type_envs
open Core


let rec typecheck_type_constructor_arg
    (types_env : Ast_types.Type_name.t list)
    (constructor_arg : Ast_types.type_expr)
  : unit Or_error.t =
  
  match constructor_arg with
  | Ast_types.TECustom custom_arg_type -> 
      assert_custom_type_in_types_env (Ast_types.Type_name.of_string custom_arg_type) types_env
  | Ast_types.TEOption option_arg_type ->
      typecheck_type_constructor_arg types_env option_arg_type
  | Ast_types.TEArrow (input_type, output_type) ->
      let open Result in
      typecheck_type_constructor_arg types_env input_type
      >>= fun () -> typecheck_type_constructor_arg types_env output_type
  | _ -> Ok ()


let rec typecheck_type_constructor_args
    (types_env : Ast_types.Type_name.t list)
    (constructor_args : Ast_types.type_expr list)
  : unit Or_error.t =
  
  match constructor_args with
  | [] -> Ok ()
  | constructor_arg :: constructor_args ->
      let open Result in
      typecheck_type_constructor_arg types_env constructor_arg
      >>= fun () -> typecheck_type_constructor_args types_env constructor_args


let typecheck_type_constructor
    (types_env : Ast_types.Type_name.t list)
    (constructors_env : constructor_env_entry list)
    (constructor_type : Ast_types.Type_name.t)
    ((TTypeConstructor(_, constructor_name, constructor_args)) : Parser_ast.type_constructor)
  : constructor_env_entry list Or_error.t =
  
  let open Result in
  assert_constructor_not_in_constructors_env constructor_name constructors_env
  >>= fun () -> typecheck_type_constructor_args types_env constructor_args
  >>= fun () ->
        let constructor_env_entry = ConstructorEnvEntry(constructor_type, constructor_name, constructor_args) in
        Ok (constructor_env_entry :: constructors_env)


let rec typecheck_type_constructors 
    (types_env : Ast_types.Type_name.t list)
    (constructors_env : constructor_env_entry list) 
    (constructors_type : Ast_types.Type_name.t)
    (type_constructors : Parser_ast.type_constructor list) 
  : constructor_env_entry list Or_error.t =

  match type_constructors with
  | [] -> Ok constructors_env
  | type_constructor :: type_constructors -> 
      let open Result in
      typecheck_type_constructor types_env constructors_env constructors_type type_constructor
      >>= fun constructors_env -> typecheck_type_constructors types_env constructors_env constructors_type type_constructors


let typecheck_type_defn
    (types_env : Ast_types.Type_name.t list)
    (constructors_env : constructor_env_entry list)
    (TType(_, type_name, type_constructors) : Parsing.Parser_ast.type_defn)
  : (Ast_types.Type_name.t list * constructor_env_entry list) Or_error.t =

  let extended_types_env = type_name :: types_env in
  let open Result in
  assert_custom_type_not_in_types_env type_name types_env
  >>= fun () -> typecheck_type_constructors extended_types_env constructors_env type_name type_constructors
  >>= fun constructors_env -> Ok (extended_types_env, constructors_env)


let rec typecheck_type_defns 
    (types_env : Ast_types.Type_name.t list)
    (constructors_env : constructor_env_entry list)
    (type_defns : Parser_ast.type_defn list)
  : (Ast_types.Type_name.t list * constructor_env_entry list * Typed_ast.type_defn list) Or_error.t =

  match type_defns with
  | [] -> Ok (types_env, constructors_env, [])
  | type_defn :: type_defns ->
    let open Result in
      typecheck_type_defn types_env constructors_env type_defn
      >>= fun (types_env, constructors_env) -> typecheck_type_defns types_env constructors_env type_defns
