open Type_infer_types

val string_of_ty : ty -> string
val string_of_ty_attr : ty_attr -> string
val pprint_ty_attr : Format.formatter -> ty_attr -> unit
val pprint_typing_context : Format.formatter -> typing_context -> unit
val pprint_constraints : Format.formatter -> constr list -> unit

val pprint_type_infer_expr_verbose :
  Format.formatter ->
  verbose:bool ->
  Parsing.Parser_ast.expr ->
  typing_context ->
  ty_attr ->
  constr list ->
  unit

val pprint_type_infer_value_verbose :
  Format.formatter ->
  verbose:bool ->
  Parsing.Parser_ast.value ->
  ty_attr ->
  constr list ->
  unit

val pprint_substs : Format.formatter -> subst list -> unit
