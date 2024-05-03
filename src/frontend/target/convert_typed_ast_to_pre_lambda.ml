open Ast.Ast_types
open Core
open Typing

let rec convert_typed_ast_to_pre_lambda_value
    (constructor_tag_map : int Pre_lambda.ConstructorTagMap.t)
    (value : Typed_ast.value) : Pre_lambda.value =
  match value with
  | Unit _ -> Unit
  | Integer (_, _, n) -> Integer n
  | Boolean (_, _, b) -> Boolean b
  | Variable (_, _, v) -> Variable v
  | Constructor (_, _, constructor_name, constructor_values) ->
      let pre_lambda_constructor_values =
        List.map constructor_values
          ~f:(convert_typed_ast_to_pre_lambda_value constructor_tag_map)
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

let rec convert_typed_ast_to_pre_lambda
    (constructors_env : Type_defns_env.constructors_env)
    (constructor_tag_map : int Pre_lambda.ConstructorTagMap.t)
    (expr : Typed_ast.expr) : Pre_lambda.expr =
  let convert_value =
    convert_typed_ast_to_pre_lambda_value constructor_tag_map
  in
  match expr with
  | UnboxedSingleton (_, _, value) -> UnboxedSingleton (convert_value value)
  | UnboxedTuple (_, _, values) ->
      UnboxedTuple (List.map values ~f:convert_value)
  | Let (_, _, _, vars, vars_expr, expr) ->
      let pre_lambda_vars_expr =
        convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map
          vars_expr
      in
      let pre_lambda_expr =
        convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map
          expr
      in
      Let (vars, pre_lambda_vars_expr, pre_lambda_expr)
  | FunApp (_, _, var_function, values) ->
      let pre_lambda_values = List.map values ~f:convert_value in
      FunApp (var_function, pre_lambda_values)
  | FunCall (_, _, function_name, values) ->
      let pre_lambda_values = List.map values ~f:convert_value in
      FunCall (function_name, pre_lambda_values)
  | If (_, _, expr_cond, expr_then) ->
      let pre_lambda_expr_cond =
        convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_cond
      in
      let pre_lambda_expr_then =
        convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_then
      in
      If (pre_lambda_expr_cond, pre_lambda_expr_then)
  | IfElse (_, _, expr_cond, expr_then, expr_else) ->
      let pre_lambda_expr_cond =
        convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_cond
      in
      let pre_lambda_expr_then =
        convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_then
      in
      let pre_lambda_expr_else =
        convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_else
      in
      IfElse (pre_lambda_expr_cond, pre_lambda_expr_then, pre_lambda_expr_else)
  | Match (_, _, _, matched_var, patterns) ->
      let pre_lambda_patterns =
        List.map patterns
          ~f:
            (convert_typed_ast_to_pre_lambda_pattern constructors_env
               constructor_tag_map)
      in
      let prepare_pre_lambda_patterns_for_compilation =
        List.map pre_lambda_patterns ~f:(fun (MPattern (matched_expr, expr)) ->
            ([ matched_expr ], expr))
      in
      Pattern_matching_compiler.compile_pattern_matching NonDestructive
        constructors_env [ matched_var ]
        prepare_pre_lambda_patterns_for_compilation Pre_lambda.Raise
  | UnOp (_, _, unary_op, expr) ->
      let pre_lambda_expr =
        convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map
          expr
      in
      UnOp (unary_op, pre_lambda_expr)
  | BinaryOp (_, _, binary_op, expr_left, expr_right) ->
      let pre_lambda_expr_left =
        convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_left
      in
      let pre_lambda_expr_right =
        convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map
          expr_right
      in
      BinaryOp (binary_op, pre_lambda_expr_left, pre_lambda_expr_right)
  | Drop (_, _, _, _, expr)
  | Free (_, _, _, expr)
  | Weak (_, _, _, expr)
  | Inst (_, _, _, expr) ->
      convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map expr

and convert_typed_ast_to_pre_lambda_pattern
    (constructors_env : Type_defns_env.constructors_env)
    (constructor_tag_map : int Pre_lambda.ConstructorTagMap.t)
    (MPattern (_, _, matched_expr, expr) : Typed_ast.pattern_expr) :
    Pre_lambda.pattern_expr =
  let pre_lambda_matched_expr =
    convert_typed_ast_to_pre_lambda_matched_expr matched_expr
  in
  let pre_lambda_matched_expr_no_underscores =
    Pre_lambda.replace_underscores_with_dummy_vars pre_lambda_matched_expr
  in
  let pre_lambda_expr =
    convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map expr
  in
  MPattern (pre_lambda_matched_expr_no_underscores, pre_lambda_expr)

and convert_typed_ast_to_pre_lambda_matched_expr
    (matched_expr : Typed_ast.matched_expr) : Pre_lambda.matched_expr =
  match matched_expr with
  | MUnderscore _ -> MUnderscore
  | MVariable (_, _, matched_var) -> MVariable matched_var
  | MConstructor (_, _, constructor_name, constructor_matched_exprs) ->
      let pre_lambda_constructor_matched_exprs =
        List.map constructor_matched_exprs
          ~f:convert_typed_ast_to_pre_lambda_matched_expr
      in
      MConstructor (constructor_name, pre_lambda_constructor_matched_exprs)

let convert_typed_ast_to_pre_lambda_function_defn
    (constructors_env : Type_defns_env.constructors_env)
    (constructor_tag_map : int Pre_lambda.ConstructorTagMap.t)
    (TFun (_, _, _, _, function_name, function_params, function_body) :
      Typed_ast.function_defn) : Pre_lambda.function_defn =
  let pre_lambda_function_params =
    List.map function_params ~f:(fun (TParam (_, param, _)) -> param)
  in
  let pre_lambda_function_body =
    convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map
      function_body
  in
  TFun (function_name, pre_lambda_function_params, pre_lambda_function_body)

let convert_typed_ast_to_pre_lambda_program
    (constructors_env : Type_defns_env.constructors_env)
    (constructor_tag_map : int Pre_lambda.ConstructorTagMap.t)
    (TProg (_, _, typed_function_defn, typed_main_expr_option) :
      Typed_ast.program) : Pre_lambda.program =
  let pre_lambda_function_defns =
    List.map typed_function_defn
      ~f:
        (convert_typed_ast_to_pre_lambda_function_defn constructors_env
           constructor_tag_map)
  in
  let pre_lambda_main_expr_option =
    match typed_main_expr_option with
    | None -> None
    | Some main_expr ->
        Some
          (convert_typed_ast_to_pre_lambda constructors_env constructor_tag_map
             main_expr)
  in
  TProg
    (constructor_tag_map, pre_lambda_function_defns, pre_lambda_main_expr_option)
