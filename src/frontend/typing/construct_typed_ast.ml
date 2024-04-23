open Core
open Type_infer_types
open Result

let rec construct_typed_ast_value (pretyped_value : Pretyped_ast.value)
    (substs : subst list) (substs_unique : subst_unique list) :
    Typed_ast.value Or_error.t =
  match pretyped_value with
  | Pretyped_ast.Unit (loc, ty_attr) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type -> Ok (Typed_ast.Unit (loc, ast_type))
  | Pretyped_ast.Integer (loc, ty_attr, i) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type -> Ok (Typed_ast.Integer (loc, ast_type, i))
  | Pretyped_ast.Boolean (loc, ty_attr, b) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type -> Ok (Typed_ast.Boolean (loc, ast_type, b))
  | Pretyped_ast.Variable (loc, ty_attr, var_name) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type -> Ok (Typed_ast.Variable (loc, ast_type, var_name))
  | Pretyped_ast.Constructor (loc, ty_attr, constructor_name, pretyped_exprs) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      let typed_exprs =
        List.map pretyped_exprs ~f:(fun pretyped_expr ->
            Or_error.ok_exn
              (construct_typed_ast_value pretyped_expr substs substs_unique))
      in
      Ok (Typed_ast.Constructor (loc, ast_type, constructor_name, typed_exprs))

let rec construct_typed_ast_expr (pretyped_expr : Pretyped_ast.expr)
    (substs : subst list) (substs_unique : subst_unique list) :
    Typed_ast.expr Or_error.t =
  match pretyped_expr with
  | UnboxedSingleton (loc, ty_attr, value) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      construct_typed_ast_value value substs substs_unique
      >>= fun value_typed ->
      Ok (Typed_ast.UnboxedSingleton (loc, ast_type, value_typed))
  | UnboxedTuple (loc, ty_attr, values) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      let values_typed =
        List.map values ~f:(fun value ->
            Or_error.ok_exn
              (construct_typed_ast_value value substs substs_unique))
      in
      Ok (Typed_ast.UnboxedTuple (loc, ast_type, values_typed))
  | Pretyped_ast.Let (loc, ty_attr, var_names, pretyped_var_expr, pretyped_expr)
    ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      construct_typed_ast_expr pretyped_var_expr substs substs_unique
      >>= fun typed_var_expr ->
      construct_typed_ast_expr pretyped_expr substs substs_unique
      >>= fun typed_expr ->
      Ok (Typed_ast.Let (loc, ast_type, var_names, typed_var_expr, typed_expr))
  | Pretyped_ast.FunApp (loc, ty_attr, var_name, pretyped_vars) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      let typed_values =
        List.map pretyped_vars ~f:(fun pretyped_var ->
            Or_error.ok_exn
              (construct_typed_ast_value pretyped_var substs substs_unique))
      in
      Ok (Typed_ast.FunApp (loc, ast_type, var_name, typed_values))
  | Pretyped_ast.FunCall
      (loc, ty_attr, function_name, pretyped_function_arg_values) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      let typed_function_args_values =
        List.map pretyped_function_arg_values
          ~f:(fun pretyped_function_arg_value ->
            Or_error.ok_exn
              (construct_typed_ast_value pretyped_function_arg_value substs
                 substs_unique))
      in
      Ok
        (Typed_ast.FunCall
           (loc, ast_type, function_name, typed_function_args_values))
  | Pretyped_ast.If (loc, ty_attr, pretyped_cond_expr, pretyped_then_expr) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      construct_typed_ast_expr pretyped_cond_expr substs substs_unique
      >>= fun typed_cond_expr ->
      construct_typed_ast_expr pretyped_then_expr substs substs_unique
      >>= fun typed_then_expr ->
      Ok (Typed_ast.If (loc, ast_type, typed_cond_expr, typed_then_expr))
  | Pretyped_ast.IfElse
      (loc, ty_attr, pretyped_cond_expr, pretyped_then_expr, pretyped_else_expr)
    ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      construct_typed_ast_expr pretyped_cond_expr substs substs_unique
      >>= fun typed_cond_expr ->
      construct_typed_ast_expr pretyped_then_expr substs substs_unique
      >>= fun typed_then_expr ->
      construct_typed_ast_expr pretyped_else_expr substs substs_unique
      >>= fun typed_else_expr ->
      Ok
        (Typed_ast.IfElse
           (loc, ast_type, typed_cond_expr, typed_then_expr, typed_else_expr))
  | Pretyped_ast.Match
      (loc, ty_attr, var_ty_attr, var_name, pretyped_pattern_exprs) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique var_ty_attr)
        loc
      >>= fun var_type ->
      let typed_pattern_exprs =
        List.map pretyped_pattern_exprs ~f:(fun pretyped_pattern_expr ->
            Or_error.ok_exn
              (construct_typed_ast_pattern pretyped_pattern_expr substs
                 substs_unique))
      in
      Ok
        (Typed_ast.Match (loc, ast_type, var_type, var_name, typed_pattern_exprs))
  | Pretyped_ast.UnOp (loc, ty_attr, unary_op, pretyped_expr) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      construct_typed_ast_expr pretyped_expr substs substs_unique
      >>= fun typed_expr ->
      Ok (Typed_ast.UnOp (loc, ast_type, unary_op, typed_expr))
  | Pretyped_ast.BinaryOp
      (loc, ty_attr, binary_op, pretyped_expr_left, pretyped_expr_right) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      construct_typed_ast_expr pretyped_expr_left substs substs_unique
      >>= fun typed_expr_left ->
      construct_typed_ast_expr pretyped_expr_right substs substs_unique
      >>= fun typed_expr_right ->
      Ok
        (Typed_ast.BinaryOp
           (loc, ast_type, binary_op, typed_expr_left, typed_expr_right))
  | Pretyped_ast.Drop (loc, ty_attr, var_ty_attr, var_name, pretyped_expr) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique var_ty_attr)
        loc
      >>= fun var_type ->
      construct_typed_ast_expr pretyped_expr substs substs_unique
      >>= fun typed_expr ->
      Ok (Typed_ast.Drop (loc, ast_type, var_type, var_name, typed_expr))
  | Pretyped_ast.Free (loc, ty_attr, k, pretyped_expr) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      construct_typed_ast_expr pretyped_expr substs substs_unique
      >>= fun typed_expr -> Ok (Typed_ast.Free (loc, ast_type, k, typed_expr))
  | Pretyped_ast.Weak (loc, ty_attr, k, pretyped_expr) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      construct_typed_ast_expr pretyped_expr substs substs_unique
      >>= fun typed_expr -> Ok (Typed_ast.Weak (loc, ast_type, k, typed_expr))
  | Pretyped_ast.Inst (loc, ty_attr, k, pretyped_expr) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      construct_typed_ast_expr pretyped_expr substs substs_unique
      >>= fun typed_expr -> Ok (Typed_ast.Inst (loc, ast_type, k, typed_expr))

and construct_typed_ast_pattern
    (pretyped_pattern_expr : Pretyped_ast.pattern_expr) (substs : subst list)
    (substs_unique : subst_unique list) : Typed_ast.pattern_expr Or_error.t =
  let (Pretyped_ast.MPattern
        (loc, ty_attr, pretyped_matched_expr, pretyped_pattern_expr)) =
    pretyped_pattern_expr
  in
  convert_ty_attr_to_ast_type (ty_attr_subst substs substs_unique ty_attr) loc
  >>= fun ast_type ->
  construct_typed_ast_matched pretyped_matched_expr substs substs_unique
  >>= fun typed_matched_expr ->
  construct_typed_ast_expr pretyped_pattern_expr substs substs_unique
  >>= fun typed_pattern_expr ->
  Ok
    (Typed_ast.MPattern (loc, ast_type, typed_matched_expr, typed_pattern_expr))

and construct_typed_ast_matched
    (pretyped_matched_expr : Pretyped_ast.matched_expr) (substs : subst list)
    (substs_unique : subst_unique list) : Typed_ast.matched_expr Or_error.t =
  match pretyped_matched_expr with
  | Pretyped_ast.MUnderscore (loc, ty_attr) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type -> Ok (Typed_ast.MUnderscore (loc, ast_type))
  | Pretyped_ast.MVariable (loc, ty_attr, var_name) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type -> Ok (Typed_ast.MVariable (loc, ast_type, var_name))
  | Pretyped_ast.MConstructor
      (loc, ty_attr, constructor_name, pretyped_matched_exprs) ->
      convert_ty_attr_to_ast_type
        (ty_attr_subst substs substs_unique ty_attr)
        loc
      >>= fun ast_type ->
      let typed_matched_exprs =
        List.map pretyped_matched_exprs ~f:(fun pretyped_matched_expr ->
            Or_error.ok_exn
              (construct_typed_ast_matched pretyped_matched_expr substs
                 substs_unique))
      in
      Ok
        (Typed_ast.MConstructor
           (loc, ast_type, constructor_name, typed_matched_exprs))
