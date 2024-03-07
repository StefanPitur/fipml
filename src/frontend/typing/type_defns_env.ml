open Ast.Ast_types
open Core

exception TypeNotFound of string
exception TypeAlreadyExists of string
exception ConstructorNotFound of string
exception ConstructorAlreadyExists of string
exception ConstructorMultipleInstancesFound

type constructor_env_entry =
  | ConstructorEnvEntry of Type_name.t * Constructor_name.t * type_expr list

type types_env = Type_name.t list
type constructors_env = constructor_env_entry list

let assert_custom_type_in_types_env (loc : loc) (type_name : Type_name.t)
    (types_env : types_env) : unit Or_error.t =
  let open Result in
  if List.mem types_env type_name ~equal:Type_name.( = ) then Ok ()
  else
    let error_string =
      Fmt.str "%s. Type %s not defined" (string_of_loc loc)
        (Type_name.to_string type_name)
    in
    Or_error.of_exn (TypeNotFound error_string)

let assert_custom_type_not_in_types_env (loc : loc) (type_name : Type_name.t)
    (types_env : types_env) : unit Or_error.t =
  let open Result in
  if not (List.mem types_env type_name ~equal:Type_name.( = )) then Ok ()
  else
    let error_string =
      Fmt.str "%s. Duplicate definition of type %s" (string_of_loc loc)
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
  | TEUnit _ | TEInt _ | TEBool _ | TEPoly _ -> Ok ()
  | TEOption (_, type_expr) -> assert_type_defined type_expr types_env
  | TEArrow (_, in_type_expr, out_type_expr) ->
      let open Result in
      assert_type_defined in_type_expr types_env >>= fun _ ->
      assert_type_defined out_type_expr types_env
  | TECustom (loc, custom_type_name) ->
      assert_custom_type_in_types_env loc custom_type_name types_env
