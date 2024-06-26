open Ast.Ast_types
open Core

exception TypeExpressionShouldBePolymorphicVar of string
exception TypeNotFound of string
exception TypeAlreadyExists of string
exception ConstructorNotFound of string
exception ConstructorAlreadyExists of string
exception ConstructorMultipleInstancesFound

type constructor_env_entry =
  | ConstructorEnvEntry of Type_name.t * Constructor_name.t * type_expr list

type constructors_env = constructor_env_entry list

type types_env_entry =
  | TypesEnvEntry of typ list * uniqueness list * type_expr list * Type_name.t

type types_env = types_env_entry list

let filter_types_env_by_type_signature
    (TypesEnvEntry
       ( typ_poly_params,
         uniqueness_poly_params,
         type_expr_poly_params,
         type_name ) :
      types_env_entry) (types_env : types_env) : types_env =
  List.filter types_env
    ~f:(fun
        (TypesEnvEntry
          ( typ_poly_params_entry,
            uniqueness_poly_params_entry,
            type_expr_poly_params_entry,
            type_name_entry ))
      ->
      Type_name.( = ) type_name_entry type_name
      && List.length typ_poly_params = List.length typ_poly_params_entry
      && List.length uniqueness_poly_params
         = List.length uniqueness_poly_params_entry
      && List.length type_expr_poly_params
         = List.length type_expr_poly_params_entry)

let assert_custom_type_in_types_env (loc : loc)
    (TypesEnvEntry
       ( typ_poly_params,
         uniqueness_poly_params,
         type_expr_poly_params,
         type_name ) as types_env_entry :
      types_env_entry) (types_env : types_env) : unit Or_error.t =
  let open Result in
  let matched_types =
    filter_types_env_by_type_signature types_env_entry types_env
  in
  match matched_types with
  | [] ->
      let typ_poly_ids = List.map ~f:string_of_typ typ_poly_params in
      let uniqueness_poly_ids =
        List.map ~f:string_of_uniqueness uniqueness_poly_params
      in
      let type_expr_poly_ids =
        List.map ~f:string_of_type type_expr_poly_params
      in
      let error_string =
        Fmt.str "%s. Type (%s ; %s ; %s) %s not defined" (string_of_loc loc)
          (String.concat ~sep:", " typ_poly_ids)
          (String.concat ~sep:", " uniqueness_poly_ids)
          (String.concat ~sep:", " type_expr_poly_ids)
          (Type_name.to_string type_name)
      in
      Or_error.of_exn (TypeNotFound error_string)
  | _ -> Ok ()

let assert_custom_type_not_in_types_env (loc : loc)
    (TypesEnvEntry
       ( typ_poly_params,
         uniqueness_poly_params,
         type_expr_poly_params,
         type_name ) as types_env_entry :
      types_env_entry) (types_env : types_env) : unit Or_error.t =
  let open Result in
  let matched_types =
    filter_types_env_by_type_signature types_env_entry types_env
  in
  match matched_types with
  | [] -> Ok ()
  | _ ->
      let typ_poly_ids = List.map ~f:string_of_typ typ_poly_params in
      let uniqueness_poly_ids =
        List.map ~f:string_of_uniqueness uniqueness_poly_params
      in
      let type_expr_poly_ids =
        List.map ~f:string_of_type type_expr_poly_params
      in
      let error_string =
        Fmt.str "%s. Duplicate definition of type (%s ; %s ; %s) %s"
          (string_of_loc loc)
          (String.concat ~sep:", " typ_poly_ids)
          (String.concat ~sep:", " uniqueness_poly_ids)
          (String.concat ~sep:", " type_expr_poly_ids)
          (Type_name.to_string type_name)
      in
      Or_error.of_exn (TypeAlreadyExists error_string)

let get_custom_type_entry_by_name (loc : loc) (types_env : types_env)
    (type_name : Type_name.t) : types_env_entry Or_error.t =
  match
    List.filter types_env ~f:(fun (TypesEnvEntry (_, _, _, type_entry_name)) ->
        Type_name.( = ) type_entry_name type_name)
  with
  | [] ->
      let error_string =
        Fmt.str "%s. Type with name %s not found" (string_of_loc loc)
          (Type_name.to_string type_name)
      in
      Or_error.of_exn (TypeNotFound error_string)
  | [ type_env_entry ] -> Ok type_env_entry
  | _ ->
      let error_string =
        Fmt.str "%s. Duplicate type names found for %s" (string_of_loc loc)
          (Type_name.to_string type_name)
      in
      Or_error.of_exn (TypeAlreadyExists error_string)

let filter_constructors_env_by_name (constructor_name : Constructor_name.t)
    (constructors_env : constructors_env) : constructors_env =
  List.filter constructors_env
    ~f:(fun (ConstructorEnvEntry (_, constructor, _)) ->
      Constructor_name.( = ) constructor_name constructor)

let assert_constructor_in_constructors_env (loc : loc)
    (constructor_name : Constructor_name.t)
    (constructors_env : constructors_env) : unit Or_error.t =
  let matched_constructors =
    filter_constructors_env_by_name constructor_name constructors_env
  in
  match matched_constructors with
  | [] ->
      let error_string =
        Fmt.str "%s. Constructor %s not defined" (string_of_loc loc)
          (Constructor_name.to_string constructor_name)
      in
      Or_error.of_exn (ConstructorNotFound error_string)
  | _ -> Ok ()

let assert_constructor_not_in_constructors_env (loc : loc)
    (constructor_name : Constructor_name.t)
    (constructors_env : constructors_env) : unit Or_error.t =
  let matched_constructors =
    filter_constructors_env_by_name constructor_name constructors_env
  in
  match matched_constructors with
  | [] -> Ok ()
  | _ ->
      let error_string =
        Fmt.str "%s. Duplicate definition of constructor %s" (string_of_loc loc)
          (Constructor_name.to_string constructor_name)
      in
      Or_error.of_exn (ConstructorAlreadyExists error_string)

let get_constructor_by_name (loc : loc) (constructor_name : Constructor_name.t)
    (constructors_env : constructors_env) : constructor_env_entry Or_error.t =
  let matched_constructors =
    filter_constructors_env_by_name constructor_name constructors_env
  in
  match matched_constructors with
  | [] ->
      let error_string =
        Fmt.str "%s. Constructor %s not defined" (string_of_loc loc)
          (Constructor_name.to_string constructor_name)
      in
      Or_error.of_exn (ConstructorNotFound error_string)
  | [ matched_constructor ] -> Ok matched_constructor
  | _ -> Or_error.of_exn ConstructorMultipleInstancesFound

let rec assert_type_defined (type_expr : type_expr) (types_env : types_env) :
    unit Or_error.t =
  match type_expr with
  | TPoly _ -> Ok ()
  | TAttr (_, typ, _) -> assert_typ_defined typ types_env

and assert_typ_defined (typ : typ) (types_env : types_env) : unit Or_error.t =
  match typ with
  | TEUnit _ | TEInt _ | TEBool _ | TEPoly _ -> Ok ()
  | TEArrow (_, in_type_expr, out_type_expr) ->
      let open Result in
      assert_type_defined in_type_expr types_env >>= fun _ ->
      assert_type_defined out_type_expr types_env
  | TECustom
      ( loc,
        typ_poly_params,
        uniqueness_poly_params,
        type_expr_poly_params,
        custom_type_name ) ->
      let open Result in
      assert_custom_type_in_types_env loc
        (TypesEnvEntry
           ( typ_poly_params,
             uniqueness_poly_params,
             type_expr_poly_params,
             custom_type_name ))
        types_env
      >>= fun () ->
      List.iter typ_poly_params ~f:(fun typ_poly_param ->
          Or_error.ok_exn (assert_typ_defined typ_poly_param types_env));
      List.iter type_expr_poly_params ~f:(fun type_expr_poly_param ->
          Or_error.ok_exn (assert_type_defined type_expr_poly_param types_env));
      Ok ()
  | TETuple (_, type_exprs) ->
      Ok
        (List.iter type_exprs ~f:(fun type_expr ->
             Or_error.ok_exn (assert_type_defined type_expr types_env)))
