open Type_infer

val pprint_ty : Format.formatter -> ty -> unit

val pprint_typing_context : Format.formatter -> typing_context -> unit

val pprint_constraints : Format.formatter -> constr list -> unit