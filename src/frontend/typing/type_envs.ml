open Ast.Ast_types
open Core

exception TypeNotFound of string
exception TypeAlreadyExists of string

exception ConstructorNotFound of string
exception ConstructorAlreadyExists of string

type constructor_env_entry =
  | ConstructorEnvEntry of type_expr * Constructor_name.t * type_expr list

let assert_custom_type_in_types_env (type_name : Type_name.t) (types_env : Type_name.t list) : unit Or_error.t = 
  let open Result in
  if List.mem types_env type_name ~equal:Type_name.(=) then 
    Ok () 
  else
    let error_string = "Type " ^ (Type_name.to_string type_name) ^ " could not be found in the defined types." in
    Or_error.of_exn (TypeNotFound(error_string))

let assert_custom_type_not_in_types_env (type_name : Type_name.t) (types_env : Type_name.t list) : unit Or_error.t =
  let open Result in
  if not (List.mem types_env type_name ~equal:Type_name.(=)) then
    Ok () 
  else
    let error_string = "Type " ^ (Type_name.to_string type_name) ^ " has been already defined." in
    Or_error.of_exn (TypeAlreadyExists(error_string))

let assert_constructor_in_constructors_env (constructor_name : Constructor_name.t) (constructors_env : constructor_env_entry list) : unit Or_error.t =
  let matched_constructors = 
    List.filter
      constructors_env 
      ~f:(fun (ConstructorEnvEntry(_, constructor, _)) -> Constructor_name.(=) constructor_name constructor)
  in

  match matched_constructors with
  | [] ->
    let error_string = "Constructor " ^ (Constructor_name.to_string constructor_name) ^ " has not been previously defined" in
    Or_error.of_exn (ConstructorNotFound(error_string))
  | _ -> Ok ()

let assert_constructor_not_in_constructors_env (constructor_name : Constructor_name.t) (constructors_env : constructor_env_entry list) : unit Or_error.t =
  let matched_constructors = 
    List.filter
      constructors_env 
      ~f:(fun (ConstructorEnvEntry(_, constructor, _)) -> Constructor_name.(=) constructor_name constructor)
  in

  match matched_constructors with
  | [] -> Ok ()
  | _ ->
    let error_string = "Constructor " ^ (Constructor_name.to_string constructor_name) ^ " has been already defined." in
    Or_error.of_exn (ConstructorAlreadyExists(error_string))
