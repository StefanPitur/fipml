open Type_infer_types

val string_of_ty : ty -> string
val pprint_ty : Format.formatter -> ty -> unit
val pprint_typing_context : Format.formatter -> typing_context -> unit
val pprint_constraints : Format.formatter -> constr list -> unit

val pprint_type_infer_expr_verbose :
  Format.formatter ->
  verbose:bool ->
  Parsing.Parser_ast.expr ->
  typing_context ->
  ty ->
  constr list ->
  unit

val pprint_type_infer_block_expr_verbose :
  Format.formatter ->
  verbose:bool ->
  Parsing.Parser_ast.block_expr ->
  typing_context ->
  ty ->
  constr list ->
  unit

val pprint_substs : Format.formatter -> subst list -> unit
