open Core

val target_program :
  Pre_lambda.program ->
  int Pre_lambda.ConstructorTagMap.t ->
  Ident.t ->
  Lambda.lambda Or_error.t
