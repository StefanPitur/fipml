open Ast
open Core

exception TypeExpressionShouldBePolymorphicVar of string
exception TypeNotFound of string
exception TypeAlreadyExists of string
exception ConstructorNotFound of string
exception ConstructorAlreadyExists of string
exception ConstructorMultipleInstancesFound

type constructor_env_entry =
  | ConstructorEnvEntry of
      Ast_types.Type_name.t
      * Ast_types.Constructor_name.t
      * Ast_types.type_expr list

type constructors_env = constructor_env_entry list

type types_env_entry =
  | TypesEnvEntry of Ast_types.type_expr list * Ast_types.Type_name.t

type types_env = types_env_entry list

val assert_type_defined : Ast_types.type_expr -> types_env -> unit Or_error.t

val assert_custom_type_in_types_env :
  Ast_types.loc -> types_env_entry -> types_env -> unit Or_error.t

val assert_custom_type_not_in_types_env :
  Ast_types.loc -> types_env_entry -> types_env -> unit Or_error.t

val get_custom_type_entry_by_name :
  Ast_types.loc ->
  types_env ->
  Ast_types.Type_name.t ->
  types_env_entry Or_error.t

val assert_constructor_in_constructors_env :
  Ast_types.loc ->
  Ast_types.Constructor_name.t ->
  constructor_env_entry list ->
  unit Or_error.t

val assert_constructor_not_in_constructors_env :
  Ast_types.loc ->
  Ast_types.Constructor_name.t ->
  constructor_env_entry list ->
  unit Or_error.t

val get_constructor_by_name :
  Ast_types.loc ->
  Ast_types.Constructor_name.t ->
  constructor_env_entry list ->
  constructor_env_entry Or_error.t
