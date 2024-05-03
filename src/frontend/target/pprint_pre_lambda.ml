open Ast.Ast_types
open Core
open Pre_lambda

let indent_tab = "    "

(* Pretty-printing Type Definition *)
let rec pprint_pre_lambda_function_defn ppf ~indent
    (TFun (function_name, function_params, function_body)) =
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%sFunction Name: %s@." indent
    (Function_name.to_string function_name);
  Fmt.pf ppf "%sParam List:@." indent;
  List.iter function_params ~f:(fun function_param ->
      Fmt.pf ppf "%sParam: %s@." sub_expr_indent
        (Var_name.to_string function_param));
  Fmt.pf ppf "%sFunction Body:@." indent;
  pprint_pre_lambda_expr ppf ~indent:sub_expr_indent function_body

and pprint_pre_lambda_matched_expr ppf ~indent pre_lambda_matched_expr =
  let print_pre_lambda_matched_expr =
    Fmt.pf ppf "%sMatchedExpr - %s@." indent
  in
  let sub_expr_indent = indent ^ indent_tab in
  match pre_lambda_matched_expr with
  | MUnderscore -> print_pre_lambda_matched_expr "Underscore"
  | MVariable var ->
      print_pre_lambda_matched_expr (Fmt.str "Var %s" (Var_name.to_string var))
  | MConstructor (constructor_name, pre_lambda_matched_exprs) ->
      print_pre_lambda_matched_expr
        (Constructor_name.to_string constructor_name);
      List.iter
        ~f:(pprint_pre_lambda_matched_expr ppf ~indent:sub_expr_indent)
        pre_lambda_matched_exprs

and pprint_pre_lambda_pattern_exprs ppf ~indent = function
  | [] ->
      raise
        (Invalid_argument
           "Match/DMatch expressions should have at least one pattern matching")
  | pre_lambda_pattern_exprs ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter
        ~f:(fun (MPattern (pre_lambda_matched_expr, pre_lambda_expr)) ->
          Fmt.pf ppf "%sPatternExpr@." indent;
          pprint_pre_lambda_matched_expr ppf ~indent:sub_expr_indent
            pre_lambda_matched_expr;
          Fmt.pf ppf "%sPatternMatchExpr@." indent;
          pprint_pre_lambda_expr ppf ~indent:sub_expr_indent pre_lambda_expr)
        pre_lambda_pattern_exprs

and string_of_constructor_kind = function
  | Atom -> "Atom"
  | NonAtom -> "NonAtom"

and string_of_match_kind = function
  | Destructive -> "Destructive"
  | NonDestructive -> "NonDestructive"

and pprint_pre_lambda_value ppf ~indent value =
  let print_value = Fmt.pf ppf "%sValue: %s@." indent in
  let sub_value_indent = indent_tab ^ indent in
  match value with
  | Unit -> print_value "Unit"
  | Integer i -> print_value (Fmt.str "Int: %d" i)
  | Boolean b -> print_value (Fmt.str "Bool: %b" b)
  | Variable var -> print_value (Fmt.str "Var: %s" (Var_name.to_string var))
  | Constructor
      (constructor_kind, constructor_tag, constructor_name, constructor_args) ->
      print_value
        (Fmt.str "Constructor - %s - %d : %s"
           (string_of_constructor_kind constructor_kind)
           constructor_tag
           (Constructor_name.to_string constructor_name));
      List.iter constructor_args
        ~f:(pprint_pre_lambda_value ppf ~indent:sub_value_indent)

and pprint_pre_lambda_expr ppf ~indent expr =
  let print_expr = Fmt.pf ppf "%sExpr: %s@." indent in
  let sub_expr_indent = indent ^ indent_tab in
  match expr with
  | UnboxedSingleton value ->
      print_expr "UnboxedSingleton";
      pprint_pre_lambda_value ppf ~indent:sub_expr_indent value
  | UnboxedTuple values ->
      print_expr "UnboxedTuple";
      List.iter ~f:(pprint_pre_lambda_value ppf ~indent:sub_expr_indent) values
  | Let (var_names, var_pre_lambda_expr, pre_lambda_expr) ->
      let var_names_strings = List.map ~f:Var_name.to_string var_names in
      print_expr
        (Fmt.str "Let vars: (%s) = "
           (String.concat ~sep:", " var_names_strings));
      pprint_pre_lambda_expr ppf ~indent:sub_expr_indent var_pre_lambda_expr;
      print_expr (Fmt.str "Let expr :");
      pprint_pre_lambda_expr ppf ~indent:sub_expr_indent pre_lambda_expr
  | FunApp (function_var, values) ->
      print_expr "FunApp :";
      Fmt.pf ppf "%sFunctionVar: %s@." sub_expr_indent
        (Var_name.to_string function_var);
      List.iter values ~f:(pprint_pre_lambda_value ppf ~indent:sub_expr_indent)
  | FunCall (function_name, values) ->
      print_expr "FunCall :";
      Fmt.pf ppf "%sFunction Name: %s@." sub_expr_indent
        (Function_name.to_string function_name);
      List.iter values ~f:(pprint_pre_lambda_value ppf ~indent:sub_expr_indent)
  | If (cond_expr, then_expr) ->
      print_expr "If";
      pprint_pre_lambda_expr ppf ~indent:sub_expr_indent cond_expr;
      Fmt.pf ppf "%sThen@." indent;
      pprint_pre_lambda_expr ppf ~indent:sub_expr_indent then_expr
  | IfElse (cond_expr, then_expr, else_expr) ->
      print_expr "IfElse";
      pprint_pre_lambda_expr ppf ~indent:sub_expr_indent cond_expr;
      Fmt.pf ppf "%sThen@." indent;
      pprint_pre_lambda_expr ppf ~indent:sub_expr_indent then_expr;
      Fmt.pf ppf "%sElse@." indent;
      pprint_pre_lambda_expr ppf ~indent:sub_expr_indent else_expr
  | Match (match_kind, var_name, pre_lambda_pattern_exprs) ->
      print_expr (Fmt.str "Match - %s" (string_of_match_kind match_kind));
      Fmt.pf ppf "%sMatch Var: %s@." sub_expr_indent
        (Var_name.to_string var_name);
      pprint_pre_lambda_pattern_exprs ppf ~indent:sub_expr_indent
        pre_lambda_pattern_exprs
  | UnOp (unary_op, pre_lambda_expr) ->
      print_expr (string_of_unary_op unary_op);
      pprint_pre_lambda_expr ppf ~indent:sub_expr_indent pre_lambda_expr
  | BinaryOp (binary_op, pre_lambda_expr_left, pre_lambda_expr_right) ->
      print_expr (string_of_binary_op binary_op);
      pprint_pre_lambda_expr ppf ~indent:sub_expr_indent pre_lambda_expr_left;
      pprint_pre_lambda_expr ppf ~indent:sub_expr_indent pre_lambda_expr_right
  | Raise -> print_expr "Raise"

and pprint_pre_lambda_program ppf
    (TProg (constructor_tag_map, function_defns, main_expr_optional)) =
  pprint_constructor_tag_map constructor_tag_map;
  Fmt.pf ppf "Program@.";
  List.iter
    ~f:(pprint_pre_lambda_function_defn ppf ~indent:indent_tab)
    function_defns;
  match main_expr_optional with
  | None -> ()
  | Some main_expr ->
      Fmt.pf ppf "%sMain@." indent_tab;
      pprint_pre_lambda_expr ppf ~indent:indent_tab main_expr

and pprint_constructor_tag_map (constructor_tag_map : int ConstructorTagMap.t) :
    unit =
  Map.iteri constructor_tag_map ~f:(fun ~key ~data ->
      Fmt.pf Fmt.stdout "Key: %s - Data: %d@."
        (Constructor_name.to_string key)
        data)
