open Core
open Pprint_type_infer
open Pretyped_ast

let indent_tab = "    "

let rec pprint_pretyped_expr (ppf : Format.formatter) ~(indent : string)
    (pretyped_expr : expr) : unit =
  let print_expr = Fmt.pf ppf "%sPretyped Expr: %s@." indent in
  let sub_expr_indent = indent ^ indent_tab in
  match pretyped_expr with
  | Unit (_, ty) -> print_expr (Fmt.str "Unit - %s" (string_of_ty ty))
  | Integer (_, ty, i) ->
      print_expr (Fmt.str "Integer %s - %s" (string_of_int i) (string_of_ty ty))
  | Boolean (_, ty, b) ->
      print_expr
        (Fmt.str "Boolean %s - %s" (string_of_bool b) (string_of_ty ty))
  | Option (_, ty, o) -> (
      match o with
      | None -> print_expr (Fmt.str "Option None - %s" (string_of_ty ty))
      | Some pretyped_expr ->
          print_expr (Fmt.str "Option Some - %s" (string_of_ty ty));
          pprint_pretyped_expr ppf ~indent:sub_expr_indent pretyped_expr)
  | Variable (_, ty, var_name) ->
      print_expr
        (Fmt.str "Var %s - %s"
           (Ast.Ast_types.Var_name.to_string var_name)
           (string_of_ty ty))
  | Constructor (_, ty, constructor_name, pretyped_constructor_args) ->
      print_expr
        (Fmt.str "Constructor %s - %s"
           (Ast.Ast_types.Constructor_name.to_string constructor_name)
           (string_of_ty ty));
      pprint_pretyped_contructor_args ppf ~indent:sub_expr_indent
        pretyped_constructor_args
  | Let (_, ty_var, var_name, pretyped_var_expr, ty_expr, pretyped_expr) ->
      print_expr
        (Fmt.str "Let var %s - %s"
           (Ast.Ast_types.Var_name.to_string var_name)
           (string_of_ty ty_var));
      pprint_pretyped_expr ppf ~indent:sub_expr_indent pretyped_var_expr;
      print_expr (Fmt.str "Let Expr - %s" (string_of_ty ty_expr));
      pprint_pretyped_expr ppf ~indent:sub_expr_indent pretyped_expr
  | FunApp (_, ty, function_name, function_args) ->
      print_expr (Fmt.str "FunApp - %s" (string_of_ty ty));
      Fmt.pf ppf "%sFunction: %s@." sub_expr_indent
        (Ast.Ast_types.Function_name.to_string function_name);
      pprint_pretyped_function_args ppf ~indent:sub_expr_indent function_args
  | If (_, ty, cond_expr, then_expr) ->
      print_expr (Fmt.str "If - %s" (string_of_ty ty));
      pprint_pretyped_expr ppf ~indent:sub_expr_indent cond_expr;
      pprint_pretyped_block_expr ppf ~indent:sub_expr_indent ~block_name:"Then"
        then_expr
  | IfElse (_, ty, cond_expr, then_expr, else_expr) ->
      print_expr (Fmt.str "IfElse - %s" (string_of_ty ty));
      pprint_pretyped_expr ppf ~indent:sub_expr_indent cond_expr;
      pprint_pretyped_block_expr ppf ~indent:sub_expr_indent ~block_name:"Then"
        then_expr;
      pprint_pretyped_block_expr ppf ~indent:sub_expr_indent ~block_name:"Else"
        else_expr
  | Match (_, ty, var_name, pretyped_pattern_exprs) ->
      print_expr (Fmt.str "Match - %s" (string_of_ty ty));
      Fmt.pf ppf "%sMatch Var: %s@." sub_expr_indent
        (Ast.Ast_types.Var_name.to_string var_name);
      pprint_pretyped_pattern_exprs ppf ~indent:sub_expr_indent
        pretyped_pattern_exprs
  | DMatch (_, ty, var_name, pretyped_pattern_exprs) ->
      print_expr (Fmt.str "DMatch - %s" (string_of_ty ty));
      Fmt.pf ppf "%sDMatch Var: %s@." sub_expr_indent
        (Ast.Ast_types.Var_name.to_string var_name);
      pprint_pretyped_pattern_exprs ppf ~indent:sub_expr_indent
        pretyped_pattern_exprs
  | UnOp (_, ty, unary_op, pretyped_expr) ->
      print_expr
        (Fmt.str "%s - %s"
           (Ast.Ast_types.string_of_unary_op unary_op)
           (string_of_ty ty));
      pprint_pretyped_expr ppf ~indent:sub_expr_indent pretyped_expr
  | BinaryOp (_, ty, binary_op, pretyped_expr_left, pretyped_expr_right) ->
      print_expr
        (Fmt.str "%s - %s"
           (Ast.Ast_types.string_of_binary_op binary_op)
           (string_of_ty ty));
      pprint_pretyped_expr ppf ~indent:sub_expr_indent pretyped_expr_left;
      pprint_pretyped_expr ppf ~indent:sub_expr_indent pretyped_expr_right
  | _ -> raise (Invalid_argument "Invalid argument for pprint_pretyped")

and pprint_pretyped_block_expr (ppf : Format.formatter) ~(indent : string)
    ~(block_name : string) (pretyped_block_expr : block_expr) : unit =
  let (Pretyped_ast.Block (_, pretyped_block_expr_ty, pretyped_exprs)) =
    pretyped_block_expr
  in
  Fmt.pf ppf "%sPretyped %s Block Expr Type - %s@." indent block_name
    (string_of_ty pretyped_block_expr_ty);
  List.iter pretyped_exprs ~f:(fun pretyped_expr ->
      pprint_pretyped_expr ppf ~indent:(indent ^ indent_tab) pretyped_expr)

and pprint_pretyped_contructor_args (ppf : Format.formatter) ~(indent : string)
    (pretyped_contructor_args : expr list) : unit =
  match pretyped_contructor_args with
  | [] -> Fmt.pf ppf "%s()@." indent
  | pretyped_contructor_args ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter pretyped_contructor_args ~f:(fun pretyped_contructor_arg ->
          Fmt.pf ppf "%sConstructorArg@." indent;
          pprint_pretyped_expr ppf ~indent:sub_expr_indent
            pretyped_contructor_arg)

and pprint_pretyped_function_args (ppf : Format.formatter) ~(indent : string)
    (pretyped_function_args : expr list) : unit =
  match pretyped_function_args with
  | [] -> Fmt.pf ppf "%s()@." indent
  | function_args ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter function_args ~f:(fun function_arg ->
          Fmt.pf ppf "%sFunctionArg@." indent;
          pprint_pretyped_expr ppf ~indent:sub_expr_indent function_arg)

and pprint_pretyped_pattern_exprs (ppf : Format.formatter) ~(indent : string)
    (pretyped_pattern_exprs : pattern_expr list) : unit =
  match pretyped_pattern_exprs with
  | [] ->
      raise
        (Invalid_argument
           "Match/DMatch expressions should have at least one pattern matching")
  | pretyped_pattern_exprs ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter pretyped_pattern_exprs
        ~f:(fun
            (MPattern (_, ty, pretyped_matched_expr, pretyped_block_expr)) ->
          Fmt.pf ppf "%sPatternExpr - %s@." indent (string_of_ty ty);
          pprint_pretyped_matched_expr ppf ~indent:sub_expr_indent
            pretyped_matched_expr;
          pprint_pretyped_block_expr ppf ~indent:sub_expr_indent
            ~block_name:"PatternBlockExpr" pretyped_block_expr)

and pprint_pretyped_matched_expr (ppf : Format.formatter) ~(indent : string)
    (pretyped_matched_expr : matched_expr) : unit =
  let print_pretyped_matched_expr =
    Fmt.pf ppf "%sPretyped MatchedExpr - %s: %s@." indent
  in
  let sub_expr_indent = indent ^ indent_tab in
  match pretyped_matched_expr with
  | MUnderscore (_, ty) ->
      print_pretyped_matched_expr (string_of_ty ty) "Underscore"
  | MVariable (_, ty, var_name) ->
      print_pretyped_matched_expr (string_of_ty ty)
        (Fmt.str "Var %s" (Ast.Ast_types.Var_name.to_string var_name))
  | MConstructor (_, ty, constructor_name, pretyped_matched_exprs) ->
      print_pretyped_matched_expr (string_of_ty ty)
        (Ast.Ast_types.Constructor_name.to_string constructor_name);
      List.iter pretyped_matched_exprs
        ~f:(pprint_pretyped_matched_expr ppf ~indent:sub_expr_indent)
  | MOption (_, ty, pretyped_matched_expr_option) -> (
      match pretyped_matched_expr_option with
      | None -> print_pretyped_matched_expr (string_of_ty ty) "Option None"
      | Some pretyped_matched_expr ->
          print_pretyped_matched_expr (string_of_ty ty) "Option Some";
          pprint_pretyped_matched_expr ppf ~indent:sub_expr_indent
            pretyped_matched_expr)
  | _ -> raise (Invalid_argument "Invalid argument for pprint_pretyped")
