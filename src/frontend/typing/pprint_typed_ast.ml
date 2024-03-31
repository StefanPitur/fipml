open Ast.Ast_types
open Typed_ast

let indent_tab = "    "

(* Pretty-printing Type Definition *)
let rec pprint_typed_defn ppf ~indent
    (TType (_, _, type_name, type_constructors)) =
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%sType Name: %s@." indent (Type_name.to_string type_name);
  Fmt.pf ppf "%sType Constructors:@." indent;
  List.iter
    (pprint_typed_constructor ppf ~indent:sub_expr_indent)
    type_constructors

and pprint_typed_constructor ppf ~indent
    (TTypeConstructor (_, _, constructor_name, type_exprs)) =
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%sType Constructor Name: %s@." indent
    (Constructor_name.to_string constructor_name);
  List.iter
    (Ast.Pprint_ast_types.pprint_type_expr ppf ~indent:sub_expr_indent)
    type_exprs

and pprint_typed_function_defn ppf ~indent
    (TFun
      ( _,
        function_return_type,
        fip_option,
        function_name,
        function_args,
        function_body )) =
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%s%sFunction Name: %s@." indent
    (string_of_fip_option fip_option)
    (Function_name.to_string function_name);
  Fmt.pf ppf "%sReturn Type: %s@." indent (string_of_type function_return_type);
  Fmt.pf ppf "%sParam List:@." indent;
  Ast.Pprint_ast_types.pprint_params ppf ~indent:sub_expr_indent function_args;
  Fmt.pf ppf "%sFunction Body:@." indent;
  pprint_typed_expr ppf ~indent:sub_expr_indent function_body

and pprint_typed_constructor_args ppf ~indent = function
  | [] -> Fmt.pf ppf "%s()@." indent
  | typed_constructor_args ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter
        (fun typed_constructor_arg ->
          Fmt.pf ppf "%sConstructorArg@." sub_expr_indent;
          pprint_typed_value ppf ~indent:sub_expr_indent typed_constructor_arg)
        typed_constructor_args

and pprint_typed_function_args ppf ~indent = function
  | [] -> Fmt.pf ppf "%s()@." indent
  | typed_function_args ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter
        (fun typed_function_arg ->
          Fmt.pf ppf "%sFunctionArg@." indent;
          pprint_typed_expr ppf ~indent:sub_expr_indent typed_function_arg)
        typed_function_args

and pprint_typed_matched_expr ppf ~indent typed_matched_expr =
  let print_typed_matched_expr =
    Fmt.pf ppf "%sTyped MatchedExpr - %s : %s@." indent
  in
  let sub_expr_indent = indent ^ indent_tab in
  match typed_matched_expr with
  | MUnderscore (_, type_expr) ->
      print_typed_matched_expr (string_of_type type_expr) "Underscore"
  | MVariable (_, type_expr, var_name) ->
      print_typed_matched_expr (string_of_type type_expr)
        (Fmt.str "Var %s" (Var_name.to_string var_name))
  | MConstructor (_, type_expr, constructor_name, typed_matched_exprs) ->
      print_typed_matched_expr (string_of_type type_expr)
        (Constructor_name.to_string constructor_name);
      List.iter
        (pprint_typed_matched_expr ppf ~indent:sub_expr_indent)
        typed_matched_exprs

and pprint_typed_pattern_exprs ppf ~indent = function
  | [] ->
      raise
        (Invalid_argument
           "Match/DMatch expressions should have at least one pattern matching")
  | typed_pattern_exprs ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter
        (fun (MPattern (_, type_expr, typed_matched_expr, typed_expr)) ->
          Fmt.pf ppf "%sPatternExpr - %s@." indent (string_of_type type_expr);
          pprint_typed_matched_expr ppf ~indent:sub_expr_indent
            typed_matched_expr;
          Fmt.pf ppf "%sPatternMatchExpr@." indent;
          pprint_typed_expr ppf ~indent:sub_expr_indent typed_expr)
        typed_pattern_exprs

and pprint_typed_value ppf ~indent value =
  let print_value = Fmt.pf ppf "%sValue: %s@." indent in
  let sub_value_indent = indent_tab ^ indent in
  match value with
  | Unit (_, type_expr) ->
      print_value (Fmt.str "Unit - %s" (string_of_type type_expr))
  | Integer (_, type_expr, i) ->
      print_value (Fmt.str "Int: %d - %s" i (string_of_type type_expr))
  | Boolean (_, type_expr, b) ->
      print_value (Fmt.str "Bool: %b - %s" b (string_of_type type_expr))
  | Variable (_, type_expr, var_name) ->
      print_value
        (Fmt.str "Var: %s - %s"
           (Var_name.to_string var_name)
           (string_of_type type_expr))
  | Constructor (_, type_expr, constructor_name, constructor_args) ->
      print_value
        (Fmt.str "Constructor: %s - %s"
           (Constructor_name.to_string constructor_name)
           (string_of_type type_expr));
      pprint_typed_constructor_args ppf ~indent:sub_value_indent
        constructor_args

and pprint_typed_expr ppf ~indent expr =
  let print_expr = Fmt.pf ppf "%sTyped Expr: %s@." indent in
  let sub_expr_indent = indent ^ indent_tab in
  match expr with
  | UnboxedSingleton (_, type_expr, value) ->
      print_expr (Fmt.str "UnboxedSingleton - %s" (string_of_type type_expr));
      pprint_typed_value ppf ~indent:sub_expr_indent value
  | UnboxedTuple (_, type_expr, values) ->
      print_expr (Fmt.str "UnboxedTuple - %s" (string_of_type type_expr));
      List.iter (pprint_typed_value ppf ~indent:sub_expr_indent) values
  | Let (_, type_expr, var_names, var_typed_expr, typed_expr) ->
      let var_names_strings = List.map Var_name.to_string var_names in
      print_expr
        (Fmt.str "Let vars: (%s) = " (String.concat ", " var_names_strings));
      pprint_typed_expr ppf ~indent:sub_expr_indent var_typed_expr;
      print_expr (Fmt.str "Let expr - %s" (string_of_type type_expr));
      pprint_typed_expr ppf ~indent:sub_expr_indent typed_expr
  | FunApp (_, type_expr, function_var, function_args) ->
      print_expr (Fmt.str "FunApp - %s" (string_of_type type_expr));
      Fmt.pf ppf "%sFunctionVar: %s@." sub_expr_indent
        (Var_name.to_string function_var);
      pprint_typed_function_args ppf ~indent:sub_expr_indent function_args
  | FunCall (_, type_expr, function_name, function_args) ->
      print_expr (Fmt.str "FunCall - %s" (string_of_type type_expr));
      Fmt.pf ppf "%sFunction Name: %s@." sub_expr_indent
        (Function_name.to_string function_name);
      pprint_typed_function_args ppf ~indent:sub_expr_indent function_args
  | If (_, type_expr, cond_expr, then_expr) ->
      print_expr (Fmt.str "If - %s" (string_of_type type_expr));
      pprint_typed_expr ppf ~indent:sub_expr_indent cond_expr;
      Fmt.pf ppf "%sThen@." indent;
      pprint_typed_expr ppf ~indent:sub_expr_indent then_expr
  | IfElse (_, type_expr, cond_expr, then_expr, else_expr) ->
      print_expr (Fmt.str "IfElse - %s" (string_of_type type_expr));
      pprint_typed_expr ppf ~indent:sub_expr_indent cond_expr;
      Fmt.pf ppf "%sThen@." indent;
      pprint_typed_expr ppf ~indent:sub_expr_indent then_expr;
      Fmt.pf ppf "%sElse@." indent;
      pprint_typed_expr ppf ~indent:sub_expr_indent else_expr
  | Match (_, type_expr, var_name, typed_pattern_exprs) ->
      print_expr (Fmt.str "Match - %s" (string_of_type type_expr));
      Fmt.pf ppf "%sMatch Var: %s@." sub_expr_indent
        (Var_name.to_string var_name);
      pprint_typed_pattern_exprs ppf ~indent:sub_expr_indent typed_pattern_exprs
  | UnOp (_, type_expr, unary_op, typed_expr) ->
      print_expr
        (Fmt.str "%s - %s"
           (string_of_unary_op unary_op)
           (string_of_type type_expr));
      pprint_typed_expr ppf ~indent:sub_expr_indent typed_expr
  | BinaryOp (_, type_expr, binary_op, typed_expr_left, typed_expr_right) ->
      print_expr
        (Fmt.str "%s - %s"
           (string_of_binary_op binary_op)
           (string_of_type type_expr));
      pprint_typed_expr ppf ~indent:sub_expr_indent typed_expr_left;
      pprint_typed_expr ppf ~indent:sub_expr_indent typed_expr_right
  | Drop (_, type_expr, dropped_var_name, expr) ->
      print_expr
        (Fmt.str "Drop %s - %s Expr:"
           (Var_name.to_string dropped_var_name)
           (string_of_type type_expr));
      pprint_typed_expr ppf ~indent:sub_expr_indent expr
  | Free (_, type_expr, freed_reuse_credit_size, expr) ->
      print_expr (Fmt.str "Free - %s" (string_of_type type_expr));
      pprint_typed_value ppf ~indent:sub_expr_indent freed_reuse_credit_size;
      Fmt.pf ppf "%sFree Expr@." indent;
      pprint_typed_expr ppf ~indent:sub_expr_indent expr

and pprint_typed_program ppf
    (TProg (type_defns, program_type, function_defns, main_expr_optional)) =
  Fmt.pf ppf "Typed Program - %s@." (string_of_type program_type);
  List.iter (pprint_typed_defn ppf ~indent:indent_tab) type_defns;
  List.iter (pprint_typed_function_defn ppf ~indent:indent_tab) function_defns;
  match main_expr_optional with
  | None -> ()
  | Some main_expr ->
      Fmt.pf ppf "%sTyped Main@." indent_tab;
      pprint_typed_expr ppf ~indent:indent_tab main_expr
