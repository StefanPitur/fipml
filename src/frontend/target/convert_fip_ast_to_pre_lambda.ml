open Core
open Typing

let rec convert_fip_ast_to_pre_lambda_value
    (constructor_tag_map : int Pre_lambda.ConstructorTagMap.t)
    (value : Fip_ast.value) : Pre_lambda.value =
  match value with
  | Unit _ -> Unit
  | Integer (_, _, _, _, n) -> Integer n
  | Boolean (_, _, _, _, b) -> Boolean b
  | Variable (_, _, _, _, v) -> Variable v
  | Constructor (_, _, _, _, constructor_name, constructor_values) ->
      let pre_lambda_constructor_values =
        List.map constructor_values
          ~f:(convert_fip_ast_to_pre_lambda_value constructor_tag_map)
      in
      let constructor_tag_map =
        Map.find_exn constructor_tag_map constructor_name
      in
      if List.length pre_lambda_constructor_values = 0 then
        Constructor (Atom, constructor_tag_map, constructor_name, [])
      else
        Constructor
          ( NonAtom,
            constructor_tag_map,
            constructor_name,
            pre_lambda_constructor_values )

let rec convert_fip_ast_to_pre_lambda
    (constructors_env : Type_defns_env.constructors_env)
    (constructor_tag_map : int Pre_lambda.ConstructorTagMap.t)
    (expr : Fip_ast.expr) : Pre_lambda.expr =
  let convert_value = convert_fip_ast_to_pre_lambda_value constructor_tag_map in
  match expr with
  | UnboxedSingleton (_, _, _, _, value) ->
      UnboxedSingleton (convert_value value)
  | UnboxedTuple (_, _, _, _, values) ->
      UnboxedTuple (List.map values ~f:convert_value)
  | Let (_, _, _, _, vars, vars_expr, expr) ->
      let pre_lambda_vars_expr =
        convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map
          vars_expr
      in
      let pre_lambda_expr =
        convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map expr
      in
      Let (vars, pre_lambda_vars_expr, pre_lambda_expr)
  | FunApp (_, _, _, _, var_function, values) ->
      FunApp (var_function, List.map values ~f:convert_value)
  | FunCall (_, _, _, _, function_name, values) ->
      FunCall (function_name, List.map values ~f:convert_value)
  | If (_, _, _, _, expr_cond, expr_then) ->
      let pre_lambda_expr_cond =
        convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_cond
      in
      let pre_lambda_expr_then =
        convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_then
      in
      If (pre_lambda_expr_cond, pre_lambda_expr_then)
  | IfElse (_, _, _, _, expr_cond, expr_then, expr_else) ->
      let pre_lambda_expr_cond =
        convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_cond
      in
      let pre_lambda_expr_then =
        convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_then
      in
      let pre_lambda_expr_else =
        convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_else
      in
      IfElse (pre_lambda_expr_cond, pre_lambda_expr_then, pre_lambda_expr_else)
  | Match (_, _, _, _, matched_var, patterns) ->
      let pre_lambda_patterns =
        List.map patterns
          ~f:
            (convert_fip_ast_to_pre_lambda_pattern constructors_env
               constructor_tag_map)
      in
      let prepare_pre_lambda_patterns_for_compilation =
        List.map pre_lambda_patterns ~f:(fun (MPattern (matched_expr, expr)) ->
            ([ matched_expr ], expr))
      in
      Pattern_matching_compiler.compile_pattern_matching NonDestructive
        constructors_env [ matched_var ]
        prepare_pre_lambda_patterns_for_compilation Pre_lambda.Raise
  | DMatch (_, _, _, _, matched_var, patterns) ->
      let pre_lambda_patterns =
        List.map patterns
          ~f:
            (convert_fip_ast_to_pre_lambda_pattern constructors_env
               constructor_tag_map)
      in
      let prepare_pre_lambda_patterns_for_compilation =
        List.map pre_lambda_patterns ~f:(fun (MPattern (matched_expr, expr)) ->
            ([ matched_expr ], expr))
      in
      Pattern_matching_compiler.compile_pattern_matching Destructive
        constructors_env [ matched_var ]
        prepare_pre_lambda_patterns_for_compilation Pre_lambda.Raise
  | UnOp (_, _, _, _, unary_op, expr) ->
      let pre_lambda_expr =
        convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map expr
      in
      UnOp (unary_op, pre_lambda_expr)
  | BinaryOp (_, _, _, _, binary_op, expr_left, expr_right) ->
      let pre_lambda_expr_left =
        convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_left
      in
      let pre_lambda_expr_right =
        convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_right
      in
      BinaryOp (binary_op, pre_lambda_expr_left, pre_lambda_expr_right)
  | Inst (_, _, _, _, k, expr) ->
      let pre_lambda_expr =
        convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map expr
      in
      Inst (k, pre_lambda_expr)
  | Free (_, _, _, _, k, expr) ->
      let pre_lambda_expr =
        convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map expr
      in
      Free (k, pre_lambda_expr)
  | Drop (_, _, _, _, _, expr) | Weak (_, _, _, _, _, expr) ->
      convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map expr

and convert_fip_ast_to_pre_lambda_pattern
    (constructors_env : Type_defns_env.constructors_env)
    (constructor_tag_map : int Pre_lambda.ConstructorTagMap.t)
    (MPattern (_, _, _, _, matched_expr, expr) : Fip_ast.pattern_expr) :
    Pre_lambda.pattern_expr =
  let pre_lambda_matched_expr =
    convert_fip_ast_to_pre_lambda_matched_expr constructors_env matched_expr
  in
  let pre_lambda_matched_expr_no_underscores =
    Pre_lambda.replace_underscores_with_dummy_vars pre_lambda_matched_expr
  in
  let pre_lambda_expr =
    convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map expr
  in
  MPattern (pre_lambda_matched_expr_no_underscores, pre_lambda_expr)

and convert_fip_ast_to_pre_lambda_matched_expr
    (constructors_env : Type_defns_env.constructors_env)
    (matched_expr : Typed_ast.matched_expr) : Pre_lambda.matched_expr =
  match matched_expr with
  | MUnderscore _ -> MUnderscore
  | MVariable (_, _, matched_var) -> MVariable matched_var
  | MConstructor (_, _, constructor_name, constructor_matched_exprs) ->
      let pre_lambda_constructor_matched_exprs =
        List.map constructor_matched_exprs
          ~f:(convert_fip_ast_to_pre_lambda_matched_expr constructors_env)
      in
      MConstructor (constructor_name, pre_lambda_constructor_matched_exprs)

let convert_fip_ast_to_pre_lambda_function_defn
    (constructors_env : Type_defns_env.constructors_env)
    (constructor_tag_map : int Pre_lambda.ConstructorTagMap.t)
    (TFun (_, _, function_name, function_params, function_body) :
      Fip_ast.function_defn) : Pre_lambda.function_defn =
  let pre_lambda_function_params =
    List.map function_params ~f:(fun (TParam (param, _)) -> param)
  in
  let pre_lambda_function_body =
    convert_fip_ast_to_pre_lambda constructors_env constructor_tag_map
      function_body
  in
  TFun
    ( Pre_lambda.Fip,
      function_name,
      pre_lambda_function_params,
      pre_lambda_function_body )

let convert_fip_ast_to_pre_lambda_function_defns
    (constructors_env : Type_defns_env.constructors_env)
    (constructor_tag_map : int Pre_lambda.ConstructorTagMap.t)
    (fip_ast_function_defns : Fip_ast.function_defn list) :
    Pre_lambda.function_defn list =
  List.map fip_ast_function_defns
    ~f:
      (convert_fip_ast_to_pre_lambda_function_defn constructors_env
         constructor_tag_map)
