open Ast
open Core

exception TypeNotFound of string
exception TypeAlreadyExists of string
exception ConstructorNotFound of string
exception ConstructorAlreadyExists of string
exception ConstructorMultipleInstancesFound

type constructor_env_entry =
  | ConstructorEnvEntry of
      Ast_types.Type_name.t * Ast_types.Constructor_name.t * Ast_types.type_expr list

type types_env = Ast_types.Type_name.t list
type constructors_env = constructor_env_entry list

val assert_custom_type_in_types_env
  :  Ast_types.loc
  -> Ast_types.Type_name.t
  -> Ast_types.Type_name.t list
  -> unit Or_error.t

val assert_custom_type_not_in_types_env
  :  Ast_types.loc
  -> Ast_types.Type_name.t
  -> Ast_types.Type_name.t list
  -> unit Or_error.t

val assert_constructor_in_constructors_env
  :  Ast_types.loc
  -> Ast_types.Constructor_name.t
  -> constructor_env_entry list
  -> unit Or_error.t

val assert_constructor_not_in_constructors_env
  :  Ast_types.loc
  -> Ast_types.Constructor_name.t
  -> constructor_env_entry list
  -> unit Or_error.t

val get_constructor_by_name
  :  Ast_types.loc
  -> Ast_types.Constructor_name.t
  -> constructor_env_entry list
  -> constructor_env_entry Or_error.t
