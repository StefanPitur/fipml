open Ast.Ast_types
open Typing

val compile_pattern_matching :
  Type_defns_env.constructors_env ->
  Var_name.t list ->
  (Typed_ast.matched_expr list * Typed_ast.expr) list ->
  Typed_ast.expr ->
  Typed_ast.expr

val fresh_var : unit -> Var_name.t

val replace_underscores_with_dummy_vars :
  Typed_ast.matched_expr -> Typed_ast.matched_expr
