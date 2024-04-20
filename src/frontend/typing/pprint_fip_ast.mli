val pprint_fip_expr : Format.formatter -> indent:string -> Fip_ast.expr -> unit
(** Pretty-prints [Fip_ast.expr] *)

val pprint_fip_value :
  Format.formatter -> indent:string -> Fip_ast.value -> unit
(** Pretty-prints [Fip_ast.value] *)

val pprint_fip_pattern_exprs :
  Format.formatter -> indent:string -> Fip_ast.pattern_expr list -> unit
