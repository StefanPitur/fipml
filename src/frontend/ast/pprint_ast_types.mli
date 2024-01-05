open Ast_types

val pprint_borrowed : Format.formatter -> indent : string -> borrowed -> unit 
val pprint_type_expr : Format.formatter -> indent : string -> type_expr -> unit
val pprint_params : Format.formatter -> indent : string -> param list -> unit
val pprint_unary_op : Format.formatter -> indent : string -> unary_op -> unit
val pprint_binary_op : Format.formatter -> indent : string -> binary_op -> unit