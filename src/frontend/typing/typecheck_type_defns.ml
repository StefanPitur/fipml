open Ast
open Core
open Parsing
open Type_defns_env

exception UndefinedPolymorphicTypeVariable of string
exception NonUniquePolymorphicTypeVariable of string

let assert_poly_parameters_unique (poly_params : Ast_types.poly list) :
    unit Or_error.t =
  match
    List.find_a_dup poly_params ~compare:(fun poly_param_1 poly_param_2 ->
        Or_error.ok_exn
          (match (poly_param_1, poly_param_2) with
          | Poly (_, poly_id_1), Poly (_, poly_id_2) ->
              Ok (Bool.to_int (String.( <> ) poly_id_1 poly_id_2))))
  with
  | None -> Ok ()
  | Some dup_poly_param ->
      Or_error.of_exn
        (NonUniquePolymorphicTypeVariable
           (Ast_types.string_of_loc (Ast_types.get_poly_loc dup_poly_param)))

let assert_poly_parameter_defined (poly_param : Ast_types.poly)
    (defined_poly_params : Ast_types.poly list) : unit Or_error.t =
  if
    List.exists defined_poly_params ~f:(fun defined_poly_param ->
        Ast_types.equal_poly defined_poly_param poly_param)
  then Ok ()
  else
    let error_string =
      Fmt.str "%s - %s"
        (Ast_types.string_of_poly poly_param)
        (Ast_types.string_of_loc (Ast_types.get_poly_loc poly_param))
    in
    Or_error.of_exn (UndefinedPolymorphicTypeVariable error_string)

let rec typecheck_type_constructor_arg_typ (types_env : types_env)
    (typ_poly_params : Ast_types.poly list)
    (uniqueness_poly_params : Ast_types.poly list)
    (type_expr_poly_params : Ast_types.poly list)
    (constructor_typ : Ast_types.typ) : unit Or_error.t =
  match constructor_typ with
  | Ast_types.TEPoly (_, poly) ->
      assert_poly_parameter_defined poly typ_poly_params
  | Ast_types.TECustom
      (loc, typ_params, uniqueness_params, type_expr_params, custom_arg_type) ->
      List.iter typ_params ~f:(fun typ_param ->
          Or_error.ok_exn
            (typecheck_type_constructor_arg_typ types_env typ_poly_params
               uniqueness_poly_params type_expr_poly_params typ_param));
      List.iter uniqueness_params ~f:(fun uniqueness_param ->
          Or_error.ok_exn
            (typecheck_type_constructor_arg_uniqueness uniqueness_poly_params
               uniqueness_param));
      List.iter type_expr_params ~f:(fun type_expr_param ->
          Or_error.ok_exn
            (typecheck_type_constructor_arg_type_expr types_env typ_poly_params
               uniqueness_poly_params type_expr_poly_params type_expr_param));
      let custom_arg_types_env_entry =
        TypesEnvEntry
          (typ_params, uniqueness_params, type_expr_params, custom_arg_type)
      in
      assert_custom_type_in_types_env loc custom_arg_types_env_entry types_env
  | Ast_types.TEArrow (_, input_type, output_type) ->
      let open Result in
      typecheck_type_constructor_arg_type_expr types_env typ_poly_params
        uniqueness_poly_params type_expr_poly_params input_type
      >>= fun () ->
      typecheck_type_constructor_arg_type_expr types_env typ_poly_params
        uniqueness_poly_params type_expr_poly_params output_type
  | Ast_types.TETuple (_, type_exprs) ->
      Ok
        (List.iter type_exprs ~f:(fun type_expr ->
             Or_error.ok_exn
               (typecheck_type_constructor_arg_type_expr types_env
                  typ_poly_params uniqueness_poly_params type_expr_poly_params
                  type_expr)))
  | Ast_types.TEUnit _ | Ast_types.TEInt _ | Ast_types.TEBool _ -> Ok ()

and typecheck_type_constructor_arg_uniqueness
    (uniqueness_poly_params : Ast_types.poly list)
    (constructor_uniqueness : Ast_types.uniqueness) : unit Or_error.t =
  match constructor_uniqueness with
  | Ast_types.PolyUnique (_, poly_unique) ->
      assert_poly_parameter_defined poly_unique uniqueness_poly_params
  | _ -> Ok ()

and typecheck_type_constructor_arg_type_expr (types_env : types_env)
    (typ_poly_params : Ast_types.poly list)
    (uniqueness_poly_params : Ast_types.poly list)
    (type_expr_poly_params : Ast_types.poly list)
    (constructor_arg : Ast_types.type_expr) : unit Or_error.t =
  match constructor_arg with
  | Ast_types.TPoly poly ->
      assert_poly_parameter_defined poly type_expr_poly_params
  | Ast_types.TAttr (_, typ, uniqueness) ->
      let open Result in
      typecheck_type_constructor_arg_uniqueness uniqueness_poly_params
        uniqueness
      >>= fun _ ->
      typecheck_type_constructor_arg_typ types_env typ_poly_params
        uniqueness_poly_params type_expr_poly_params typ

let rec typecheck_type_constructor_args (types_env : types_env)
    (typ_poly_params : Ast_types.poly list)
    (uniqueness_poly_params : Ast_types.poly list)
    (type_expr_poly_params : Ast_types.poly list)
    (constructor_args : Ast_types.type_expr list) : unit Or_error.t =
  match constructor_args with
  | [] -> Ok ()
  | constructor_arg :: constructor_args ->
      let open Result in
      typecheck_type_constructor_arg_type_expr types_env typ_poly_params
        uniqueness_poly_params type_expr_poly_params constructor_arg
      >>= fun () ->
      typecheck_type_constructor_args types_env typ_poly_params
        uniqueness_poly_params type_expr_poly_params constructor_args

let typecheck_type_constructor (types_env : types_env)
    (constructors_env : constructors_env)
    (TType
       ( _,
         typ_poly_params,
         uniqueness_poly_params,
         type_expr_poly_params,
         custom_base_type,
         _ ) :
      Parser_ast.type_defn)
    (TTypeConstructor (loc, constructor_name, constructor_args) :
      Parser_ast.type_constructor) :
    (constructors_env * Typed_ast.type_constructor) Or_error.t =
  let open Result in
  assert_constructor_not_in_constructors_env loc constructor_name
    constructors_env
  >>= fun () ->
  let typ_polys =
    List.map typ_poly_params ~f:(fun typ_poly_param ->
        Or_error.ok_exn (Ast_types.convert_typ_to_poly typ_poly_param))
  in
  let uniqueness_polys =
    List.map uniqueness_poly_params ~f:(fun uniqueness_poly_param ->
        Or_error.ok_exn
          (Ast_types.convert_uniqueness_to_poly uniqueness_poly_param))
  in
  let type_expr_polys =
    List.map type_expr_poly_params ~f:(fun type_expr_poly_param ->
        Or_error.ok_exn
          (Ast_types.convert_type_expr_to_poly type_expr_poly_param))
  in
  typecheck_type_constructor_args types_env typ_polys uniqueness_polys
    type_expr_polys constructor_args
  >>= fun () ->
  let constructor_env_entry =
    ConstructorEnvEntry (custom_base_type, constructor_name, constructor_args)
  in
  let typed_ast_type_constructor =
    Typed_ast.TTypeConstructor
      ( loc,
        TECustom
          ( loc,
            typ_poly_params,
            uniqueness_poly_params,
            type_expr_poly_params,
            custom_base_type ),
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
        (typed_ast_type_constructors @ [ typed_ast_type_constructor ])
        custom_base_type type_constructors

let typecheck_type_defn (types_env : types_env)
    (constructors_env : constructors_env)
    (TType
       ( loc,
         typ_poly_params,
         uniqueness_poly_params,
         type_expr_poly_params,
         type_name,
         type_constructors ) as custom_base_type :
      Parsing.Parser_ast.type_defn) :
    (types_env * constructors_env * Typed_ast.type_defn) Or_error.t =
  let open Result in
  let typ_polys =
    List.map typ_poly_params ~f:(fun typ_poly_param ->
        Or_error.ok_exn (Ast_types.convert_typ_to_poly typ_poly_param))
  in
  let uniqueness_polys =
    List.map uniqueness_poly_params ~f:(fun uniqueness_poly_param ->
        Or_error.ok_exn
          (Ast_types.convert_uniqueness_to_poly uniqueness_poly_param))
  in
  let type_expr_polys =
    List.map type_expr_poly_params ~f:(fun type_expr_poly_param ->
        Or_error.ok_exn
          (Ast_types.convert_type_expr_to_poly type_expr_poly_param))
  in
  assert_poly_parameters_unique (typ_polys @ uniqueness_polys @ type_expr_polys)
  >>= fun () ->
  let type_env_entry =
    TypesEnvEntry
      (typ_poly_params, uniqueness_poly_params, type_expr_poly_params, type_name)
  in
  assert_custom_type_not_in_types_env loc type_env_entry types_env >>= fun () ->
  let extended_types_env = type_env_entry :: types_env in
  typecheck_type_constructors extended_types_env constructors_env []
    custom_base_type type_constructors
  >>= fun (constructors_env, typed_ast_type_constructors) ->
  Ok
    ( extended_types_env,
      constructors_env,
      Typed_ast.TType
        ( loc,
          Ast_types.TECustom
            ( loc,
              typ_poly_params,
              uniqueness_poly_params,
              type_expr_poly_params,
              type_name ),
          typ_poly_params,
          uniqueness_poly_params,
          type_expr_poly_params,
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
        (typed_ast_type_defns @ [ typed_ast_type_defn ])
        type_defns

let typecheck_type_defns (type_defns : Parser_ast.type_defn list) :
    (types_env * constructors_env * Typed_ast.type_defn list) Or_error.t =
  typecheck_type_defns_wrapper [] [] [] type_defns

let rec pprint_types_env (ppf : Format.formatter) (types_env : types_env) : unit
    =
  match types_env with
  | [] -> ()
  | TypesEnvEntry
      (typ_poly_params, uniqueness_poly_params, type_expr_poly_params, type_name)
    :: types_env ->
      let typ_poly_ids = List.map ~f:Ast_types.string_of_typ typ_poly_params in
      let uniqueness_poly_ids =
        List.map ~f:Ast_types.string_of_uniqueness uniqueness_poly_params
      in
      let type_expr_poly_ids =
        List.map ~f:Ast_types.string_of_type type_expr_poly_params
      in
      Fmt.pf ppf "(%s ; %s ; %s) %s@."
        (String.concat ~sep:", " typ_poly_ids)
        (String.concat ~sep:", " uniqueness_poly_ids)
        (String.concat ~sep:", " type_expr_poly_ids)
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
