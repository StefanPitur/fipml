open Ast.Ast_types
open Core

exception TypeNotFound
exception TypeAlreadyExists

exception ConstructorNotFound
exception ConstructorAlreadyExists
exception ConstructorMultipleInstancesFound

type constructor_env_entry =
  | ConstructorEnvEntry of Type_name.t * Constructor_name.t * type_expr list

let assert_custom_type_in_types_env (type_name : Type_name.t) (types_env : Type_name.t list) : unit Or_error.t = 
  let open Result in
  if List.mem types_env type_name ~equal:Type_name.(=) then 
    Ok () 
  else
    Or_error.of_exn (TypeNotFound)

let assert_custom_type_not_in_types_env (type_name : Type_name.t) (types_env : Type_name.t list) : unit Or_error.t =
  let open Result in
  if not (List.mem types_env type_name ~equal:Type_name.(=)) then
    Ok () 
  else
    Or_error.of_exn (TypeAlreadyExists)

let filter_constructors_env_by_name (constructor_name : Constructor_name.t) (constructors_env : constructor_env_entry list) : constructor_env_entry list =
  List.filter
    constructors_env
    ~f: (fun (ConstructorEnvEntry(_, constructor, _)) -> Constructor_name.(=) constructor_name constructor)

let assert_constructor_in_constructors_env (constructor_name : Constructor_name.t) (constructors_env : constructor_env_entry list) : unit Or_error.t =
  let matched_constructors = filter_constructors_env_by_name constructor_name constructors_env in
  match matched_constructors with
  | [] ->
    Or_error.of_exn (ConstructorNotFound)
  | _ -> Ok ()

let assert_constructor_not_in_constructors_env (constructor_name : Constructor_name.t) (constructors_env : constructor_env_entry list) : unit Or_error.t =
  let matched_constructors = filter_constructors_env_by_name constructor_name constructors_env in
  match matched_constructors with
  | [] -> Ok ()
  | _ -> Or_error.of_exn (ConstructorAlreadyExists)

let get_constructor_by_name (constructor_name : Constructor_name.t) (constructors_env : constructor_env_entry list) : constructor_env_entry Or_error.t =
  let matched_constructors = filter_constructors_env_by_name constructor_name constructors_env in
  match matched_constructors with
  | [] -> Or_error.of_exn(ConstructorNotFound)
  | [matched_constructor] -> Ok (matched_constructor)
  | _ -> Or_error.of_exn(ConstructorMultipleInstancesFound)
