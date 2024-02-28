open Core
open Type_infer_constraints_unification
open Type_infer_types
open Result

let rec construct_typed_ast_expr (pretyped_expr : Pretyped_ast.expr)
    (substs : subst list) : Typed_ast.expr Or_error.t =
  match pretyped_expr with
  | Pretyped_ast.Unit (loc, ty) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      Ok (Typed_ast.Unit (loc, ast_type))
  | Pretyped_ast.Integer (loc, ty, i) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      Ok (Typed_ast.Integer (loc, ast_type, i))
  | Pretyped_ast.Boolean (loc, ty, b) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      Ok (Typed_ast.Boolean (loc, ast_type, b))
  | Pretyped_ast.Option (loc, ty, pretyped_expr_option) -> (
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      match pretyped_expr_option with
      | None -> Ok (Typed_ast.Option (loc, ast_type, None))
      | Some pretyped_expr ->
          construct_typed_ast_expr pretyped_expr substs
          >>= fun typed_ast_expr ->
          Ok (Typed_ast.Option (loc, ast_type, Some typed_ast_expr)))
  | Pretyped_ast.Variable (loc, ty, var_name) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      Ok (Typed_ast.Variable (loc, ast_type, var_name))
  | Pretyped_ast.Constructor (loc, ty, constructor_name, pretyped_exprs) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      let typed_exprs =
        List.map pretyped_exprs ~f:(fun pretyped_expr ->
            Or_error.ok_exn (construct_typed_ast_expr pretyped_expr substs))
      in
      Ok (Typed_ast.Constructor (loc, ast_type, constructor_name, typed_exprs))
  | Pretyped_ast.Tuple (loc, ty, pretyped_expr_left, pretyped_expr_right) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      construct_typed_ast_expr pretyped_expr_left substs
      >>= fun typed_expr_left ->
      construct_typed_ast_expr pretyped_expr_right substs
      >>= fun typed_expr_right ->
      Ok (Typed_ast.Tuple (loc, ast_type, typed_expr_left, typed_expr_right))
  | Pretyped_ast.Let
      (loc, var_ty, var_name, pretyped_var_expr, ty, pretyped_expr) ->
      convert_ty_to_ast_type (ty_subst substs var_ty) loc
      >>= fun var_ast_type ->
      construct_typed_ast_expr pretyped_var_expr substs
      >>= fun typed_var_expr ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      construct_typed_ast_expr pretyped_expr substs >>= fun typed_expr ->
      Ok
        (Typed_ast.Let
           (loc, var_ast_type, var_name, typed_var_expr, ast_type, typed_expr))
  | Pretyped_ast.FunApp (loc, ty, function_name, pretyped_exprs) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      let typed_exprs =
        List.map pretyped_exprs ~f:(fun pretyped_expr ->
            Or_error.ok_exn (construct_typed_ast_expr pretyped_expr substs))
      in
      Ok (Typed_ast.FunApp (loc, ast_type, function_name, typed_exprs))
  | Pretyped_ast.If (loc, ty, pretyped_cond_expr, pretyped_block_expr) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun cond_ast_type ->
      construct_typed_ast_expr pretyped_cond_expr substs
      >>= fun typed_cond_expr ->
      construct_typed_ast_block pretyped_block_expr substs
      >>= fun typed_block_expr ->
      Ok (Typed_ast.If (loc, cond_ast_type, typed_cond_expr, typed_block_expr))
  | Pretyped_ast.IfElse
      ( loc,
        ty,
        pretyped_cond_expr,
        pretyped_then_block_expr,
        pretyped_else_block_expr ) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun cond_ast_type ->
      construct_typed_ast_expr pretyped_cond_expr substs
      >>= fun typed_cond_expr ->
      construct_typed_ast_block pretyped_then_block_expr substs
      >>= fun typed_then_block_expr ->
      construct_typed_ast_block pretyped_else_block_expr substs
      >>= fun typed_else_block_expr ->
      Ok
        (Typed_ast.IfElse
           ( loc,
             cond_ast_type,
             typed_cond_expr,
             typed_then_block_expr,
             typed_else_block_expr ))
  | Pretyped_ast.Match (loc, ty, var_name, pretyped_pattern_exprs) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      let typed_pattern_exprs =
        List.map pretyped_pattern_exprs ~f:(fun pretyped_pattern_expr ->
            Or_error.ok_exn
              (construct_typed_ast_pattern pretyped_pattern_expr substs))
      in
      Ok (Typed_ast.Match (loc, ast_type, var_name, typed_pattern_exprs))
  | Pretyped_ast.DMatch (loc, ty, var_name, pretyped_pattern_exprs) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      let typed_pattern_exprs =
        List.map pretyped_pattern_exprs ~f:(fun pretyped_pattern_expr ->
            Or_error.ok_exn
              (construct_typed_ast_pattern pretyped_pattern_expr substs))
      in
      Ok (Typed_ast.DMatch (loc, ast_type, var_name, typed_pattern_exprs))
  | Pretyped_ast.UnOp (loc, ty, unary_op, pretyped_expr) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      construct_typed_ast_expr pretyped_expr substs >>= fun typed_expr ->
      Ok (Typed_ast.UnOp (loc, ast_type, unary_op, typed_expr))
  | Pretyped_ast.BinaryOp
      (loc, ty, binary_op, pretyped_expr_left, pretyped_expr_right) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      construct_typed_ast_expr pretyped_expr_left substs
      >>= fun typed_expr_left ->
      construct_typed_ast_expr pretyped_expr_right substs
      >>= fun typed_expr_right ->
      Ok
        (Typed_ast.BinaryOp
           (loc, ast_type, binary_op, typed_expr_left, typed_expr_right))

and construct_typed_ast_block (pretyped_block_expr : Pretyped_ast.block_expr)
    (substs : subst list) : Typed_ast.block_expr Or_error.t =
  let (Pretyped_ast.Block (loc, pretyped_block_expr_ty, pretyped_exprs)) =
    pretyped_block_expr
  in
  convert_ty_to_ast_type (ty_subst substs pretyped_block_expr_ty) loc
  >>= fun block_ast_type ->
  let typed_exprs =
    List.map pretyped_exprs ~f:(fun pretyped_expr ->
        Or_error.ok_exn (construct_typed_ast_expr pretyped_expr substs))
  in
  Ok (Typed_ast.Block (loc, block_ast_type, typed_exprs))

and construct_typed_ast_pattern
    (pretyped_pattern_expr : Pretyped_ast.pattern_expr) (substs : subst list) :
    Typed_ast.pattern_expr Or_error.t =
  let (Pretyped_ast.MPattern
        (loc, ty, pretyped_matched_expr, pretyped_block_expr)) =
    pretyped_pattern_expr
  in
  convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
  construct_typed_ast_matched pretyped_matched_expr substs
  >>= fun typed_matched_expr ->
  construct_typed_ast_block pretyped_block_expr substs
  >>= fun typed_block_expr ->
  Ok (Typed_ast.MPattern (loc, ast_type, typed_matched_expr, typed_block_expr))

and construct_typed_ast_matched
    (pretyped_matched_expr : Pretyped_ast.matched_expr) (substs : subst list) :
    Typed_ast.matched_expr Or_error.t =
  match pretyped_matched_expr with
  | Pretyped_ast.MUnderscore (loc, ty) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      Ok (Typed_ast.MUnderscore (loc, ast_type))
  | Pretyped_ast.MVariable (loc, ty, var_name) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      Ok (Typed_ast.MVariable (loc, ast_type, var_name))
  | Pretyped_ast.MTuple
      (loc, ty, pretyped_matched_expr_left, pretyped_matched_expr_right) ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      construct_typed_ast_matched pretyped_matched_expr_left substs
      >>= fun typed_matched_expr_left ->
      construct_typed_ast_matched pretyped_matched_expr_right substs
      >>= fun typed_matched_expr_right ->
      Ok
        (Typed_ast.MTuple
           (loc, ast_type, typed_matched_expr_left, typed_matched_expr_right))
  | Pretyped_ast.MConstructor (loc, ty, constructor_name, pretyped_matched_exprs)
    ->
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      let typed_matched_exprs =
        List.map pretyped_matched_exprs ~f:(fun pretyped_matched_expr ->
            Or_error.ok_exn
              (construct_typed_ast_matched pretyped_matched_expr substs))
      in
      Ok
        (Typed_ast.MConstructor
           (loc, ast_type, constructor_name, typed_matched_exprs))
  | Pretyped_ast.MOption (loc, ty, pretyped_matched_expr_option) -> (
      convert_ty_to_ast_type (ty_subst substs ty) loc >>= fun ast_type ->
      match pretyped_matched_expr_option with
      | None -> Ok (Typed_ast.MOption (loc, ast_type, None))
      | Some pretyped_matched_expr ->
          construct_typed_ast_matched pretyped_matched_expr substs
          >>= fun typed_matched_expr ->
          Ok (Typed_ast.MOption (loc, ast_type, Some typed_matched_expr)))
