open Ast.Ast_types
open Core
open Lambda
open Typing
open Typing.Type_defns_env
module ConstructorTagMap : Map.S with type Key.t = Constructor_name.t

type constructor_tag_map_entry = int
type ident_context = Ident.t Type_context_env.typing_context

val convert_types_env_to_constructors_tag :
  constructors_env -> constructor_tag_map_entry ConstructorTagMap.t

val convert_typed_ast_value :
  Typed_ast.value ->
  ident_context ->
  constructor_tag_map_entry ConstructorTagMap.t ->
  lambda Or_error.t

val convert_typed_ast_expr :
  Typed_ast.expr ->
  ident_context ->
  constructor_tag_map_entry ConstructorTagMap.t ->
  lambda Or_error.t

val convert_typed_ast_function_defn :
  Typed_ast.function_defn ->
  constructor_tag_map_entry ConstructorTagMap.t ->
  (Ident.t * lambda) Or_error.t

val convert_typed_ast_program :
  Typed_ast.program -> constructors_env -> lambda Or_error.t
