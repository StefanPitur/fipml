open Ast.Ast_types
open Typing

val compile_pattern_matching :
  Pre_lambda.match_kind ->
  Type_defns_env.constructors_env ->
  Var_name.t list ->
  (Pre_lambda.matched_expr list * Pre_lambda.expr) list ->
  Pre_lambda.expr ->
  Pre_lambda.expr
