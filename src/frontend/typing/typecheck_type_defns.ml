open Ast
open Core
open Parsing
open Type_defns_env

exception PolymorphicTypeExpressionExpected
exception UndefinedPolymorphicTypeVariable of string
exception NonUniquePolymorphicTypeVariable of string

let assert_poly_parameters_unique (poly_params : Ast_types.type_expr list) :
    unit Or_error.t =
  match
    List.find_a_dup poly_params ~compare:(fun poly_param_1 poly_param_2 ->
        Or_error.ok_exn
          (match (poly_param_1, poly_param_2) with
          | TEPoly (_, poly_id_1), TEPoly (_, poly_id_2) ->
              Ok (Bool.to_int (String.( <> ) poly_id_1 poly_id_2))
          | _ ->
              print_string "err";
              Or_error.of_exn PolymorphicTypeExpressionExpected))
  with
  | None -> Ok ()
  | Some dup_poly_param ->
      Or_error.of_exn
        (NonUniquePolymorphicTypeVariable
           (Ast_types.string_of_loc (Ast_types.get_loc dup_poly_param)))

let assert_poly_parameter_defined (poly_param : Ast_types.type_expr)
    (defined_poly_params : Ast_types.type_expr list) : unit Or_error.t =
  match poly_param with
  | Ast_types.TEPoly (loc, poly_id) -> (
      match
        List.filter defined_poly_params ~f:(fun defined_poly_param ->
            Or_error.ok_exn
              (match defined_poly_param with
              | Ast_types.TEPoly (_, defined_poly_id) ->
                  Ok (String.( = ) poly_id defined_poly_id)
              | _ -> Or_error.of_exn PolymorphicTypeExpressionExpected))
      with
      | [] ->
          Or_error.of_exn
            (UndefinedPolymorphicTypeVariable (Ast_types.string_of_loc loc))
      | _ -> Ok ())
  | _ -> Or_error.of_exn PolymorphicTypeExpressionExpected

let rec typecheck_type_constructor_arg (types_env : types_env)
    (custom_base_type_poly_params : Ast_types.type_expr list)
    (constructor_arg : Ast_types.type_expr) : unit Or_error.t =
  match constructor_arg with
  | Ast_types.TEPoly _ ->
      assert_poly_parameter_defined constructor_arg custom_base_type_poly_params
  | Ast_types.TECustom (loc, custom_arg_type_poly_params, custom_arg_type) ->
      List.iter custom_arg_type_poly_params
        ~f:(fun custom_arg_type_poly_param ->
          Or_error.ok_exn
            (typecheck_type_constructor_arg types_env
               custom_base_type_poly_params custom_arg_type_poly_param));
      let custom_arg_types_env_entry =
        TypesEnvEntry (custom_arg_type_poly_params, custom_arg_type)
      in
      assert_custom_type_in_types_env loc custom_arg_types_env_entry types_env
  | Ast_types.TEArrow (_, input_type, output_type) ->
      let open Result in
      typecheck_type_constructor_arg types_env custom_base_type_poly_params
        input_type
      >>= fun () ->
      typecheck_type_constructor_arg types_env custom_base_type_poly_params
        output_type
  | _ -> Ok ()

let rec typecheck_type_constructor_args (types_env : types_env)
    (custom_base_type_poly_params : Ast_types.type_expr list)
    (constructor_args : Ast_types.type_expr list) : unit Or_error.t =
  match constructor_args with
  | [] -> Ok ()
  | constructor_arg :: constructor_args ->
      let open Result in
      typecheck_type_constructor_arg types_env custom_base_type_poly_params
        constructor_arg
      >>= fun () ->
      typecheck_type_constructor_args types_env custom_base_type_poly_params
        constructor_args

let typecheck_type_constructor (types_env : types_env)
    (constructors_env : constructors_env)
    (TType (_, custom_base_type_poly_params, custom_base_type, _) :
      Parser_ast.type_defn)
    (TTypeConstructor (loc, constructor_name, constructor_args) :
      Parser_ast.type_constructor) :
    (constructors_env * Typed_ast.type_constructor) Or_error.t =
  let open Result in
  assert_constructor_not_in_constructors_env loc constructor_name
    constructors_env
  >>= fun () ->
  typecheck_type_constructor_args types_env custom_base_type_poly_params
    constructor_args
  >>= fun () ->
  let constructor_env_entry =
    ConstructorEnvEntry (custom_base_type, constructor_name, constructor_args)
  in
  let typed_ast_type_constructor =
    Typed_ast.TTypeConstructor
      ( loc,
        TECustom (loc, custom_base_type_poly_params, custom_base_type),
        constructor_name,
        constructor_args )
  in
  Ok (constructor_env_entry :: constructors_env, typed_ast_type_constructor)

let rec typecheck_type_constructors (types_env : types_env)
    (constructors_env : constructors_env)
    (typed_ast_type_constructors : Typed_ast.type_constructor list)
    (custom_base_type : Parser_ast.type_defn)
    (type_constructors : Parser_ast.type_constructor list) :
    (constructors_env * Typed_ast.type_constructor list) Or_error.t =
  match type_constructors with
  | [] -> Ok (constructors_env, typed_ast_type_constructors)
  | type_constructor :: type_constructors ->
      let open Result in
      typecheck_type_constructor types_env constructors_env custom_base_type
        type_constructor
      >>= fun (constructors_env, typed_ast_type_constructor) ->
      typecheck_type_constructors types_env constructors_env
        (typed_ast_type_constructor :: typed_ast_type_constructors)
        custom_base_type type_constructors

let typecheck_type_defn (types_env : types_env)
    (constructors_env : constructors_env)
    (TType (loc, type_poly_params, type_name, type_constructors) as
     custom_base_type :
      Parsing.Parser_ast.type_defn) :
    (types_env * constructors_env * Typed_ast.type_defn) Or_error.t =
  let open Result in
  assert_poly_parameters_unique type_poly_params >>= fun () ->
  let type_env_entry = TypesEnvEntry (type_poly_params, type_name) in
  let extended_types_env = type_env_entry :: types_env in
  assert_custom_type_not_in_types_env loc type_env_entry types_env >>= fun () ->
  typecheck_type_constructors extended_types_env constructors_env []
    custom_base_type type_constructors
  >>= fun (constructors_env, typed_ast_type_constructors) ->
  Ok
    ( extended_types_env,
      constructors_env,
      Typed_ast.TType
        ( loc,
          Ast_types.TECustom (loc, type_poly_params, type_name),
          type_poly_params,
          type_name,
          typed_ast_type_constructors ) )

let rec typecheck_type_defns_wrapper (types_env : types_env)
    (constructors_env : constructors_env)
    (typed_ast_type_defns : Typed_ast.type_defn list)
    (type_defns : Parser_ast.type_defn list) :
    (types_env * constructors_env * Typed_ast.type_defn list) Or_error.t =
  match type_defns with
  | [] -> Ok (types_env, constructors_env, typed_ast_type_defns)
  | type_defn :: type_defns ->
      let open Result in
      typecheck_type_defn types_env constructors_env type_defn
      >>= fun (types_env, constructors_env, typed_ast_type_defn) ->
      typecheck_type_defns_wrapper types_env constructors_env
        (typed_ast_type_defn :: typed_ast_type_defns)
        type_defns

let typecheck_type_defns (type_defns : Parser_ast.type_defn list) :
    (types_env * constructors_env * Typed_ast.type_defn list) Or_error.t =
  typecheck_type_defns_wrapper [] [] [] type_defns

let rec pprint_types_env (ppf : Format.formatter) (types_env : types_env) : unit
    =
  match types_env with
  | [] -> ()
  | TypesEnvEntry (type_poly_params, type_name) :: types_env ->
      let type_poly_ids =
        List.map ~f:Ast_types.string_of_type type_poly_params
      in
      Fmt.pf ppf "(%s) %s@."
        (String.concat ~sep:", " type_poly_ids)
        (Ast_types.Type_name.to_string type_name);
      pprint_types_env ppf types_env

let rec pprint_constructors_env (ppf : Format.formatter)
    (constructors_env : constructors_env) : unit =
  let mock_loc : Lexing.position =
    { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }
  in
  let indent = "    " in
  match constructors_env with
  | [] -> ()
  | ConstructorEnvEntry (type_name, constructor_name, constructor_params)
    :: constructors_env ->
      Fmt.pf ppf "Constructor Type : %s@."
        (Ast_types.Type_name.to_string type_name);
      Pprint_parser_ast.pprint_type_constructor ppf ~indent
        (Parser_ast.TTypeConstructor
           (mock_loc, constructor_name, constructor_params));
      pprint_constructors_env ppf constructors_env
