open Ast
open Core

exception TypeNotFound
exception TypeAlreadyExists

exception ConstructorNotFound
exception ConstructorAlreadyExists

type constructor_env_entry =
  | ConstructorEnvEntry of Ast_types.type_expr * Ast_types.Constructor_name.t * Ast_types.type_expr list

val assert_custom_type_in_types_env : Ast_types.Type_name.t -> Ast_types.Type_name.t list -> unit Or_error.t;;
val assert_custom_type_not_in_types_env : Ast_types.Type_name.t -> Ast_types.Type_name.t list -> unit Or_error.t;;

val assert_constructor_in_constructors_env : Ast_types.Constructor_name.t -> constructor_env_entry list -> unit Or_error.t;;
val assert_constructor_not_in_constructors_env : Ast_types.Constructor_name.t -> constructor_env_entry list -> unit Or_error.t;;
