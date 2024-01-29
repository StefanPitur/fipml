open Ast.Ast_types
open Core

exception TypeNotFound
exception TypeAlreadyExists

exception ConstructorNotFound
exception ConstructorAlreadyExists

type constructor_env_entry =
  | ConstructorEnvEntry of type_expr * Constructor_name.t * type_expr list

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

let assert_constructor_in_constructors_env (constructor_name : Constructor_name.t) (constructors_env : constructor_env_entry list) : unit Or_error.t =
  let matched_constructors = 
    List.filter
      constructors_env 
      ~f:(fun (ConstructorEnvEntry(_, constructor, _)) -> Constructor_name.(=) constructor_name constructor)
  in

  match matched_constructors with
  | [] ->
    Or_error.of_exn (ConstructorNotFound)
  | _ -> Ok ()

let assert_constructor_not_in_constructors_env (constructor_name : Constructor_name.t) (constructors_env : constructor_env_entry list) : unit Or_error.t =
  let matched_constructors = 
    List.filter
      constructors_env 
      ~f:(fun (ConstructorEnvEntry(_, constructor, _)) -> Constructor_name.(=) constructor_name constructor)
  in

  match matched_constructors with
  | [] -> Ok ()
  | _ -> Or_error.of_exn (ConstructorAlreadyExists)
