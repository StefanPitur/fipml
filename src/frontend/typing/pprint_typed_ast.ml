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
      (_, function_return_type, function_name, function_args, function_body)) =
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%sFunction Name: %s@." indent
    (Function_name.to_string function_name);
  Fmt.pf ppf "%sReturn Type: %s@." indent (string_of_type function_return_type);
  Fmt.pf ppf "%sParam List:@." indent;
  Ast.Pprint_ast_types.pprint_params ppf ~indent:sub_expr_indent function_args;
  pprint_typed_block_expr ppf ~indent:sub_expr_indent
    ~block_name:"Function Body" function_body

and pprint_typed_constructor_args ppf ~indent = function
  | [] -> Fmt.pf ppf "%s()@." indent
  | typed_constructor_args ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter
        (fun typed_constructor_arg ->
          Fmt.pf ppf "%sConstructorArg@." indent;
          pprint_typed_expr ppf ~indent:sub_expr_indent typed_constructor_arg)
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
  | MOption (_, type_expr, typed_matched_expr_option) -> (
      match typed_matched_expr_option with
      | None ->
          print_typed_matched_expr (string_of_type type_expr) "Option None"
      | Some typed_matched_expr ->
          print_typed_matched_expr (string_of_type type_expr) "Option Some";
          pprint_typed_matched_expr ppf ~indent:sub_expr_indent
            typed_matched_expr)
  | _ ->
      raise (Invalid_argument "Invalid argument for pprint_typed_matched_expr")

and pprint_typed_pattern_exprs ppf ~indent = function
  | [] ->
      raise
        (Invalid_argument
           "Match/DMatch expressions should have at least one pattern matching")
  | typed_pattern_exprs ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter
        (fun (MPattern (_, type_expr, typed_matched_expr, typed_block_expr)) ->
          Fmt.pf ppf "%sPatternExpr - %s@." indent (string_of_type type_expr);
          pprint_typed_matched_expr ppf ~indent:sub_expr_indent
            typed_matched_expr;
          pprint_typed_block_expr ppf ~indent:sub_expr_indent
            ~block_name:"PatternBlockExpr" typed_block_expr)
        typed_pattern_exprs

and pprint_typed_block_expr ppf ~indent ~block_name
    (Block (_, block_type, exprs)) =
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%s%s Block - %s@." indent block_name (string_of_type block_type);
  List.iter (pprint_typed_expr ppf ~indent:sub_expr_indent) exprs

and pprint_typed_expr ppf ~indent expr =
  let print_expr = Fmt.pf ppf "%sTyped Expr: %s@." indent in
  let sub_expr_indent = indent ^ indent_tab in
  match expr with
  | Unit (_, type_expr) ->
      print_expr (Fmt.str "Unit - %s" (string_of_type type_expr))
  | Integer (_, type_expr, i) ->
      print_expr (Fmt.str "Int: %d - %s" i (string_of_type type_expr))
  | Boolean (_, type_expr, b) ->
      print_expr (Fmt.str "Bool: %b - %s" b (string_of_type type_expr))
  | Option (_, type_expr, o) -> (
      match o with
      | None ->
          print_expr (Fmt.str "Option None - %s" (string_of_type type_expr))
      | Some expr ->
          print_expr (Fmt.str "Option Some - %s" (string_of_type type_expr));
          pprint_typed_expr ppf ~indent:sub_expr_indent expr)
  | Variable (_, type_expr, var_name) ->
      print_expr
        (Fmt.str "Var %s - %s"
           (Var_name.to_string var_name)
           (string_of_type type_expr))
  | Constructor (_, type_expr, constructor_name, constructor_args) ->
      print_expr
        (Fmt.str "Constructor %s - %s"
           (Constructor_name.to_string constructor_name)
           (string_of_type type_expr));
      pprint_typed_constructor_args ppf ~indent:sub_expr_indent constructor_args
  | Let (_, var_type, var_name, var_expr, type_expr, typed_expr) ->
      print_expr
        (Fmt.str "Let var %s - %s "
           (Var_name.to_string var_name)
           (string_of_type var_type));
      pprint_typed_expr ppf ~indent:sub_expr_indent var_expr;
      print_expr (Fmt.str "Let expr - %s" (string_of_type type_expr));
      pprint_typed_expr ppf ~indent:sub_expr_indent typed_expr
  | FunApp (_, type_expr, function_name, function_args) ->
      print_expr (Fmt.str "FunApp - %s" (string_of_type type_expr));
      Fmt.pf ppf "%sFunction: %s@." sub_expr_indent
        (Function_name.to_string function_name);
      pprint_typed_function_args ppf ~indent:sub_expr_indent function_args
  | If (_, type_expr, cond_expr, then_expr) ->
      print_expr (Fmt.str "If - %s" (string_of_type type_expr));
      pprint_typed_expr ppf ~indent:sub_expr_indent cond_expr;
      pprint_typed_block_expr ppf ~indent:sub_expr_indent ~block_name:"Then"
        then_expr
  | IfElse (_, type_expr, cond_expr, then_expr, else_expr) ->
      print_expr (Fmt.str "IfElse - %s" (string_of_type type_expr));
      pprint_typed_expr ppf ~indent:sub_expr_indent cond_expr;
      pprint_typed_block_expr ppf ~indent:sub_expr_indent ~block_name:"Then"
        then_expr;
      pprint_typed_block_expr ppf ~indent:sub_expr_indent ~block_name:"Else"
        else_expr
  | Match (_, type_expr, var_name, typed_pattern_exprs) ->
      print_expr (Fmt.str "Match - %s" (string_of_type type_expr));
      Fmt.pf ppf "%sMatch Var: %s@." sub_expr_indent
        (Var_name.to_string var_name);
      pprint_typed_pattern_exprs ppf ~indent:sub_expr_indent typed_pattern_exprs
  | DMatch (_, type_expr, var_name, typed_pattern_exprs) ->
      print_expr (Fmt.str "DMatch - %s" (string_of_type type_expr));
      Fmt.pf ppf "%sDMatch Var: %s@." sub_expr_indent
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
  | _ -> raise (Invalid_argument "Invalid argument for pprint_typed_expr")

and pprint_typed_program ppf
    (TProg (type_defns, _, function_defns, main_block_expr_optional)) =
  Fmt.pf ppf "Typed Program@.";
  List.iter (pprint_typed_defn ppf ~indent:indent_tab) type_defns;
  List.iter (pprint_typed_function_defn ppf ~indent:indent_tab) function_defns;
  match main_block_expr_optional with
  | None -> ()
  | Some main_block_expr ->
      pprint_typed_block_expr ppf ~indent:indent_tab ~block_name:"Typed Main"
        main_block_expr
