open Core
open Type_infer_types
module FreeSet : Set.S with type Elt.t = String.t

val type_infer :
  Type_defns_env.types_env ->
  Type_defns_env.constructors_env ->
  Functions_env.functions_env ->
  typing_context ->
  constr_unique list ->
  ty_attr option ->
  Parsing.Parser_ast.expr ->
  verbose:bool ->
  (Typed_ast.expr * subst list * subst_unique list) Or_error.t
(** Type Inference for functions and main expression *)

val generate_constraints :
  Type_defns_env.types_env ->
  Type_defns_env.constructors_env ->
  Functions_env.functions_env ->
  typing_context ->
  Parsing.Parser_ast.expr ->
  verbose:bool ->
  (typing_context
  * ty_attr
  * constr list
  * constr_unique list
  * subst list
  * Pretyped_ast.expr)
  Or_error.t
(** Call constraints generation on expression *)

val generate_constraints_value_expr :
  Type_defns_env.types_env ->
  Type_defns_env.constructors_env ->
  Functions_env.functions_env ->
  typing_context ->
  Parsing.Parser_ast.value ->
  verbose:bool ->
  (ty_attr * constr list * constr_unique list * Pretyped_ast.value) Or_error.t

val unify : constr list -> (subst list * constr_unique list) Or_error.t
val unify_unique : constr_unique list -> subst_unique list Or_error.t
val free_type_vars : ty -> FreeSet.t
val bounded_type_vars : typing_context -> FreeSet.t

val generalise :
  typing_context ->
  Ast.Ast_types.Var_name.t ->
  ty_attr ->
  typing_context Or_error.t

val instantiate :
  Ast.Ast_types.Var_name.t -> typing_context -> ty_attr Or_error.t
