val convert_typed_ast_to_pre_lambda_program :
  Typing.Type_defns_env.constructors_env ->
  int Pre_lambda.ConstructorTagMap.t ->
  Typing.Typed_ast.program ->
  Pre_lambda.program
