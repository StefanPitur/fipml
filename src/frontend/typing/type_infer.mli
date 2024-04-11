open Core

val type_infer :
  Type_defns_env.types_env ->
  Type_defns_env.constructors_env ->
  Functions_env.functions_env ->
  Type_infer_types.typing_context ->
  Parsing.Parser_ast.expr ->
  verbose:bool ->
  Typed_ast.expr Or_error.t
(** Type Inference for functions and main expression *)
