open Ast.Ast_types
open Core
open Pprint_type_infer
open Pretyped_ast

let indent_tab = "    "

let rec pprint_pretyped_expr ppf ~indent expr =
  let print_expr = Fmt.pf ppf "%sPretyped Expr: %s@." indent in
  let sub_expr_indent = indent ^ indent_tab in
  match expr with
  | UnboxedSingleton (_, ty_attr, value) ->
      print_expr (Fmt.str "UnboxedSingleton - %s" (string_of_ty_attr ty_attr));
      pprint_pretyped_value ppf ~indent:sub_expr_indent value
  | UnboxedTuple (_, ty_attr, values) ->
      print_expr (Fmt.str "UnboxedTuple - %s" (string_of_ty_attr ty_attr));
      List.iter ~f:(pprint_pretyped_value ppf ~indent:sub_expr_indent) values
  | Let (_, ty_attr, var_ty_attrs, var_names, var_typed_expr, typed_expr) ->
      let var_names_strings = List.map ~f:Var_name.to_string var_names in
      let var_ty_attrs_strings = List.map ~f:string_of_ty_attr var_ty_attrs in
      print_expr
        (Fmt.str "Let vars: (%s) : (%s) = "
           (String.concat ~sep:", " var_names_strings)
           (String.concat ~sep:", " var_ty_attrs_strings));
      pprint_pretyped_expr ppf ~indent:sub_expr_indent var_typed_expr;
      print_expr (Fmt.str "Let expr - %s" (string_of_ty_attr ty_attr));
      pprint_pretyped_expr ppf ~indent:sub_expr_indent typed_expr
  | FunApp (_, ty_attr, function_var, function_args) ->
      print_expr (Fmt.str "FunApp - %s" (string_of_ty_attr ty_attr));
      Fmt.pf ppf "%sFunctionVar: %s@." sub_expr_indent
        (Var_name.to_string function_var);
      pprint_pretyped_function_args ppf ~indent:sub_expr_indent function_args
  | FunCall (_, ty_attr, function_name, function_args) ->
      print_expr (Fmt.str "FunCall - %s" (string_of_ty_attr ty_attr));
      Fmt.pf ppf "%sFunction Name: %s@." sub_expr_indent
        (Function_name.to_string function_name);
      pprint_pretyped_function_args ppf ~indent:sub_expr_indent function_args
  | If (_, ty_attr, cond_expr, then_expr) ->
      print_expr (Fmt.str "If - %s" (string_of_ty_attr ty_attr));
      pprint_pretyped_expr ppf ~indent:sub_expr_indent cond_expr;
      Fmt.pf ppf "%sThen@." indent;
      pprint_pretyped_expr ppf ~indent:sub_expr_indent then_expr
  | IfElse (_, ty_attr, cond_expr, then_expr, else_expr) ->
      print_expr (Fmt.str "IfElse - %s" (string_of_ty_attr ty_attr));
      pprint_pretyped_expr ppf ~indent:sub_expr_indent cond_expr;
      Fmt.pf ppf "%sThen@." indent;
      pprint_pretyped_expr ppf ~indent:sub_expr_indent then_expr;
      Fmt.pf ppf "%sElse@." indent;
      pprint_pretyped_expr ppf ~indent:sub_expr_indent else_expr
  | Match (_, ty_attr, var_ty_attr, var_name, typed_pattern_exprs) ->
      print_expr (Fmt.str "Match - %s" (string_of_ty_attr ty_attr));
      Fmt.pf ppf "%sMatch Var: %s - %s@." sub_expr_indent
        (Var_name.to_string var_name)
        (string_of_ty_attr var_ty_attr);
      pprint_pretyped_pattern_exprs ppf ~indent:sub_expr_indent
        typed_pattern_exprs
  | UnOp (_, ty_attr, unary_op, typed_expr) ->
      print_expr
        (Fmt.str "%s - %s"
           (string_of_unary_op unary_op)
           (string_of_ty_attr ty_attr));
      pprint_pretyped_expr ppf ~indent:sub_expr_indent typed_expr
  | BinaryOp (_, ty_attr, binary_op, typed_expr_left, typed_expr_right) ->
      print_expr
        (Fmt.str "%s - %s"
           (string_of_binary_op binary_op)
           (string_of_ty_attr ty_attr));
      pprint_pretyped_expr ppf ~indent:sub_expr_indent typed_expr_left;
      pprint_pretyped_expr ppf ~indent:sub_expr_indent typed_expr_right
  | Drop (_, ty_attr, dropped_var_ty_attr, dropped_var_name, expr) ->
      print_expr
        (Fmt.str "Drop (%s : %s) - %s Expr:"
           (Var_name.to_string dropped_var_name)
           (string_of_ty_attr dropped_var_ty_attr)
           (string_of_ty_attr ty_attr));
      pprint_pretyped_expr ppf ~indent:sub_expr_indent expr
  | Free (_, ty_attr, k, expr) ->
      print_expr (Fmt.str "Free %i - %s" k (string_of_ty_attr ty_attr));
      Fmt.pf ppf "%sFree Expr@." indent;
      pprint_pretyped_expr ppf ~indent:sub_expr_indent expr
  | Weak (_, ty_attr, k, expr) ->
      print_expr (Fmt.str "Weak %i - %s" k (string_of_ty_attr ty_attr));
      Fmt.pf ppf "%sWeak Expr@." indent;
      pprint_pretyped_expr ppf ~indent:sub_expr_indent expr
  | Inst (_, ty_attr, k, expr) ->
      print_expr (Fmt.str "Inst %i - %s" k (string_of_ty_attr ty_attr));
      Fmt.pf ppf "%sInst Expr@." indent;
      pprint_pretyped_expr ppf ~indent:sub_expr_indent expr

and pprint_pretyped_value ppf ~indent value =
  let print_value = Fmt.pf ppf "%sValue: %s@." indent in
  let sub_value_indent = indent_tab ^ indent in
  match value with
  | Unit (_, ty_attr) ->
      print_value (Fmt.str "Unit - %s" (string_of_ty_attr ty_attr))
  | Integer (_, ty_attr, i) ->
      print_value (Fmt.str "Int: %d - %s" i (string_of_ty_attr ty_attr))
  | Boolean (_, ty_attr, b) ->
      print_value (Fmt.str "Bool: %b - %s" b (string_of_ty_attr ty_attr))
  | Variable (_, ty_attr, var_name) ->
      print_value
        (Fmt.str "Var: %s - %s"
           (Var_name.to_string var_name)
           (string_of_ty_attr ty_attr))
  | Constructor (_, ty_attr, constructor_name, constructor_args) ->
      print_value
        (Fmt.str "Constructor: %s - %s"
           (Constructor_name.to_string constructor_name)
           (string_of_ty_attr ty_attr));
      pprint_pretyped_constructor_args ppf ~indent:sub_value_indent
        constructor_args

and pprint_pretyped_constructor_args ppf ~indent = function
  | [] -> Fmt.pf ppf "%s()@." indent
  | pretyped_constructor_args ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter
        ~f:(fun pretyped_constructor_arg ->
          Fmt.pf ppf "%sConstructorArg@." sub_expr_indent;
          pprint_pretyped_value ppf ~indent:sub_expr_indent
            pretyped_constructor_arg)
        pretyped_constructor_args

and pprint_pretyped_function_args (ppf : Format.formatter) ~(indent : string)
    (pretyped_function_args : value list) : unit =
  match pretyped_function_args with
  | [] -> Fmt.pf ppf "%s()@." indent
  | function_args ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter function_args ~f:(fun function_arg ->
          Fmt.pf ppf "%sFunctionArg@." indent;
          pprint_pretyped_value ppf ~indent:sub_expr_indent function_arg)

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
        ~f:(fun (MPattern (_, ty_attr, pretyped_matched_expr, pretyped_expr)) ->
          Fmt.pf ppf "%sPatternExpr - %s@." indent (string_of_ty_attr ty_attr);
          pprint_pretyped_matched_expr ppf ~indent:sub_expr_indent
            pretyped_matched_expr;
          Fmt.pf ppf "%sPatternMatchExpr@." indent;
          pprint_pretyped_expr ppf ~indent:sub_expr_indent pretyped_expr)

and pprint_pretyped_matched_expr (ppf : Format.formatter) ~(indent : string)
    (pretyped_matched_expr : matched_expr) : unit =
  let print_pretyped_matched_expr =
    Fmt.pf ppf "%sPretyped MatchedExpr - %s: %s@." indent
  in
  let sub_expr_indent = indent ^ indent_tab in
  match pretyped_matched_expr with
  | MUnderscore (_, ty_attr) ->
      print_pretyped_matched_expr (string_of_ty_attr ty_attr) "Underscore"
  | MVariable (_, ty_attr, var_name) ->
      print_pretyped_matched_expr
        (string_of_ty_attr ty_attr)
        (Fmt.str "Var %s" (Ast.Ast_types.Var_name.to_string var_name))
  | MConstructor (_, ty_attr, constructor_name, pretyped_matched_exprs) ->
      print_pretyped_matched_expr
        (string_of_ty_attr ty_attr)
        (Ast.Ast_types.Constructor_name.to_string constructor_name);
      List.iter pretyped_matched_exprs
        ~f:(pprint_pretyped_matched_expr ppf ~indent:sub_expr_indent)
