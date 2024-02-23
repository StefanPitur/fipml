open Core
open Type_infer_types

exception UnableToUnify

val type_infer :
  Type_defns_env.types_env ->
  Type_defns_env.constructors_env ->
  Functions_env.functions_env ->
  Parsing.Parser_ast.block_expr ->
  verbose:bool ->
  unit Or_error.t
(** Type Inference for functions and main expression *)

val generate_constraints :
  Type_defns_env.constructors_env ->
  Functions_env.functions_env ->
  typing_context ->
  Parsing.Parser_ast.expr ->
  verbose:bool ->
  (typing_context * ty * constr list) Or_error.t
(** Call constraints generation on expression *)
