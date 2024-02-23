open Core

val type_infer :
  Type_defns_env.types_env ->
  Type_defns_env.constructors_env ->
  Functions_env.functions_env ->
  Parsing.Parser_ast.block_expr ->
  verbose:bool ->
  unit Or_error.t
(** Type Inference for functions and main expression *)
