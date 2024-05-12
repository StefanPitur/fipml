val convert_fip_ast_to_pre_lambda_function_defns :
  Typing.Type_defns_env.constructors_env ->
  int Pre_lambda.ConstructorTagMap.t ->
  Typing.Fip_ast.function_defn list ->
  Pre_lambda.function_defn list
