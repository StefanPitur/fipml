val pprint_typed_program : Format.formatter -> Typed_ast.program -> unit

val pprint_typed_matched_expr :
  Format.formatter -> indent:string -> Typed_ast.matched_expr -> unit

val pprint_typed_expr :
  Format.formatter -> indent:string -> Typed_ast.expr -> unit
