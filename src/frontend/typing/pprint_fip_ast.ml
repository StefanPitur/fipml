open Ast.Ast_types
open Borrowed_context
open Core
open Fip_ast
open Owned_context
open Reuse_credits

let indent_tab = "    "

let rec pprint_fip_value (ppf : Format.formatter) ~(indent : string)
    (value : value) : unit =
  let print_value = Fmt.pf ppf "%sFip Value: %s@." indent in
  let sub_value_indent = indent ^ indent_tab in
  let b, o, r = get_fip_contexts_from_value value in
  pprint_borrowed_set ppf ~indent b;
  pprint_owned_set ppf ~indent o;
  pprint_reuse_map ppf ~indent r;
  match value with
  | Unit _ -> print_value "Unit"
  | Integer (_, _, _, _, i) -> print_value (Fmt.str "Integer - %i" i)
  | Boolean (_, _, _, _, boolean) ->
      print_value (Fmt.str "Boolean - %b" boolean)
  | Variable (_, _, _, _, var) ->
      print_value (Fmt.str "Variable - %s" (Var_name.to_string var))
  | Constructor (_, _, _, _, constructor_name, constructor_args) ->
      print_value
        (Fmt.str "Constructor - %s"
           (Constructor_name.to_string constructor_name));
      List.iter constructor_args ~f:(fun constructor_arg ->
          pprint_fip_value ppf ~indent:sub_value_indent constructor_arg)

and pprint_fip_expr (ppf : Format.formatter) ~(indent : string) (expr : expr) :
    unit =
  let print_expr = Fmt.pf ppf "%sFip Expr: %s@." indent in
  let sub_expr_indent = indent ^ indent_tab in
  let b, o, r = get_fip_contexts_from_expr expr in
  pprint_borrowed_set ppf ~indent b;
  pprint_owned_set ppf ~indent o;
  pprint_reuse_map ppf ~indent r;
  match expr with
  | UnboxedSingleton (_, _, _, _, value) ->
      print_expr "UnboxedSingleton";
      pprint_fip_value ppf ~indent:sub_expr_indent value
  | UnboxedTuple (_, _, _, _, values) ->
      print_expr "UnboxedValue";
      List.iter values ~f:(fun value ->
          pprint_fip_value ppf ~indent:sub_expr_indent value)
  | Let (_, _, _, _, vars, var_expr, expr) ->
      let var_names_strings = List.map vars ~f:Var_name.to_string in
      print_expr
        (Fmt.str "Let vars: (%s) = "
           (String.concat ~sep:", " var_names_strings));
      pprint_fip_expr ppf ~indent:sub_expr_indent var_expr;
      print_expr "Let expr:";
      pprint_fip_expr ppf ~indent:sub_expr_indent expr
  | FunApp (_, _, _, _, function_var, function_args) ->
      print_expr "FunApp";
      Fmt.pf ppf "%sFunctionVar: %s@." sub_expr_indent
        (Var_name.to_string function_var);
      pprint_fip_function_args ppf ~indent:sub_expr_indent function_args
  | FunCall (_, _, _, _, function_name, function_args) ->
      print_expr "FunCall";
      Fmt.pf ppf "%sFunction Name: %s@." sub_expr_indent
        (Function_name.to_string function_name);
      pprint_fip_function_args ppf ~indent:sub_expr_indent function_args
  | If (_, _, _, _, cond_expr, then_expr) ->
      print_expr "If";
      pprint_fip_expr ppf ~indent:sub_expr_indent cond_expr;
      print_expr "Then";
      pprint_fip_expr ppf ~indent:sub_expr_indent then_expr
  | IfElse (_, _, _, _, cond_expr, then_expr, else_expr) ->
      print_expr "If";
      pprint_fip_expr ppf ~indent:sub_expr_indent cond_expr;
      print_expr "Then";
      pprint_fip_expr ppf ~indent:sub_expr_indent then_expr;
      print_expr "Else";
      pprint_fip_expr ppf ~indent:sub_expr_indent else_expr
  | Match (_, _, _, _, var_name, fip_pattern_exprs) ->
      print_expr "Match";
      Fmt.pf ppf "%sMatch Var: %s@." sub_expr_indent
        (Var_name.to_string var_name);
      pprint_fip_pattern_exprs ppf ~indent:sub_expr_indent fip_pattern_exprs
  | DMatch (_, _, _, _, var_name, fip_pattern_exprs) ->
      print_expr "DMatch";
      Fmt.pf ppf "%sMatch Var: %s@." sub_expr_indent
        (Var_name.to_string var_name);
      pprint_fip_pattern_exprs ppf ~indent:sub_expr_indent fip_pattern_exprs
  | UnOp (_, _, _, _, unary_op, expr) ->
      print_expr (string_of_unary_op unary_op);
      pprint_fip_expr ppf ~indent:sub_expr_indent expr
  | BinaryOp (_, _, _, _, binary_op, expr_left, expr_right) ->
      print_expr (string_of_binary_op binary_op);
      pprint_fip_expr ppf ~indent:sub_expr_indent expr_left;
      pprint_fip_expr ppf ~indent:sub_expr_indent expr_right
  | Drop (_, _, _, _, var, expr) ->
      print_expr (Fmt.str "Drop - %s" (Var_name.to_string var));
      pprint_fip_expr ppf ~indent:sub_expr_indent expr
  | Free (_, _, _, _, k, expr) ->
      print_expr (Fmt.str "Free - %i" k);
      pprint_fip_expr ppf ~indent:sub_expr_indent expr
  | Weak (_, _, _, _, k, expr) ->
      print_expr (Fmt.str "Weak - %i" k);
      pprint_fip_expr ppf ~indent:sub_expr_indent expr
  | Inst (_, _, _, _, k, expr) ->
      print_expr (Fmt.str "Inst - %i" k);
      pprint_fip_expr ppf ~indent:sub_expr_indent expr

and pprint_fip_function_args ppf ~indent = function
  | [] -> Fmt.pf ppf "%s()@." indent
  | fip_function_args ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter fip_function_args ~f:(fun fip_function_arg ->
          Fmt.pf ppf "%sFunctionArg@." indent;
          pprint_fip_value ppf ~indent:sub_expr_indent fip_function_arg)

and pprint_fip_pattern_exprs ppf ~indent = function
  | [] ->
      raise
        (Invalid_argument
           "Match/DMatch expressions should have at least one pattern matching")
  | fip_pattern_exprs ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter fip_pattern_exprs
        ~f:(fun (MPattern (_, b, o, r, matched_expr, fip_expr)) ->
          pprint_borrowed_set ppf ~indent b;
          pprint_owned_set ppf ~indent o;
          pprint_reuse_map ppf ~indent r;
          Fmt.pf ppf "%sPatternExpr@." indent;
          Pprint_typed_ast.pprint_typed_matched_expr ppf ~indent:sub_expr_indent
            matched_expr;
          Fmt.pf ppf "%sPatternMatchExpr@." indent;
          pprint_fip_expr ppf ~indent:sub_expr_indent fip_expr)

let pprint_fip_param ppf ~indent = function
  | TParam (param_name, param_borrowed) ->
      Fmt.pf ppf "%s%sParam: %s@." indent
        (string_of_borrowed_option param_borrowed)
        (Var_name.to_string param_name)

let pprint_fip_params ppf ~indent = function
  | [] -> Fmt.pf ppf "%sVoid@." indent
  | params -> List.iter ~f:(pprint_fip_param ppf ~indent) params

let pprint_fip_function_defn ppf ~indent
    (TFun (_, fip_option, function_name, function_args, function_body)) =
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%s%sFunction Name: %s@." indent
    (string_of_fip_option (Some fip_option))
    (Function_name.to_string function_name);
  Fmt.pf ppf "%sParam List:@." indent;
  pprint_fip_params ppf ~indent:sub_expr_indent function_args;
  Fmt.pf ppf "%sFunction Body:@." indent;
  pprint_fip_expr ppf ~indent:sub_expr_indent function_body
