open Core
open Type_infer_types

val construct_typed_ast_expr :
  Pretyped_ast.expr -> subst list -> Typed_ast.expr Or_error.t
(** Converts the pretyped_ast into a fully typed_ast by apply substitutions *)
