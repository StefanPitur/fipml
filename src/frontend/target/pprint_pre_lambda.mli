val pprint_pre_lambda_matched_expr :
  Format.formatter -> indent:string -> Pre_lambda.matched_expr -> unit

val pprint_pre_lambda_expr :
  Format.formatter -> indent:string -> Pre_lambda.expr -> unit

val pprint_pre_lambda_program : Format.formatter -> Pre_lambda.program -> unit
