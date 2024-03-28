open Ast.Ast_types
open Ast.Pprint_ast_types
open Parser_ast

let indent_tab = "    "

(* Pretty-printing Value *)
let rec pprint_value ppf ~indent value =
  let print_value = Fmt.pf ppf "%sValue: %s@." indent in
  let sub_value_indent = indent_tab ^ indent in
  match value with
  | Unit _ -> print_value "Unit"
  | Integer (_, i) -> print_value (Fmt.str "Int: %d" i)
  | Boolean (_, b) -> print_value (Fmt.str "Bool: %b" b)
  | Variable (_, var_name) ->
      print_value (Fmt.str "Var: %s" (Var_name.to_string var_name))
  | Constructor (_, constructor_name, constructor_args) ->
      print_value
        (Fmt.str "Constructor: %s"
           (Constructor_name.to_string constructor_name));
      pprint_constructor_args ppf ~indent:sub_value_indent constructor_args

and pprint_constructor_args ppf ~indent = function
  | [] -> Fmt.pf ppf "%s()@." indent
  | constructor_args ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter
        (fun constructor_arg ->
          Fmt.pf ppf "%sConstructorArg@." indent;
          pprint_value ppf ~indent:sub_expr_indent constructor_arg)
        constructor_args

(* Pretty-printing Expression *)
and pprint_expr ppf ~indent expr =
  let print_expr = Fmt.pf ppf "%sExpr: %s@." indent in
  let sub_expr_indent = indent_tab ^ indent in
  match expr with
  | UnboxedSingleton (_, value) ->
      print_expr "UnboxedSingleton";
      pprint_value ppf ~indent:sub_expr_indent value
  | UnboxedTuple (_, values) ->
      print_expr "UnboxedTuple";
      List.iter (pprint_value ppf ~indent:sub_expr_indent) values
  | Let (_, var_names, expr, in_expr) ->
      let var_names_strings = List.map Var_name.to_string var_names in
      print_expr
        (Fmt.str "Let vars: (%s) = " (String.concat ", " var_names_strings));
      pprint_expr ppf ~indent:sub_expr_indent expr;
      pprint_expr ppf ~indent:sub_expr_indent in_expr
  | FunApp (_, function_var, owned_function_args) -> (
      print_expr "FunApp";
      Fmt.pf ppf "%sFunctionVar: %s@." sub_expr_indent
        (Var_name.to_string function_var);
      Fmt.pf ppf "%sOwned Args:@." sub_expr_indent;
      match owned_function_args with
      | Some owned_function_args ->
          pprint_expr ppf ~indent:sub_expr_indent owned_function_args
      | None -> Fmt.pf ppf "%s()@." sub_expr_indent)
  | FunCall (_, function_name, borrowed_function_args, owned_function_args) -> (
      print_expr "FunCall";
      Fmt.pf ppf "%sFunction Name: %s@." sub_expr_indent
        (Function_name.to_string function_name);
      Fmt.pf ppf "%sBorrowed Args:@." sub_expr_indent;
      (match borrowed_function_args with
      | Some borrowed_function_args ->
          pprint_expr ppf ~indent:sub_expr_indent borrowed_function_args
      | None -> Fmt.pf ppf "%s()@." sub_expr_indent);
      Fmt.pf ppf "%sOwned Args:@." sub_expr_indent;
      match owned_function_args with
      | Some owned_function_args ->
          pprint_expr ppf ~indent:sub_expr_indent owned_function_args
      | None -> Fmt.pf ppf "%s()@." sub_expr_indent)
  | If (_, cond_expr, then_expr) ->
      print_expr "If";
      pprint_expr ppf ~indent:sub_expr_indent cond_expr;
      pprint_block_expr ppf ~indent:sub_expr_indent ~block_name:"Then" then_expr
  | IfElse (_, cond_expr, then_expr, else_expr) ->
      print_expr "IfElse";
      pprint_expr ppf ~indent:sub_expr_indent cond_expr;
      pprint_block_expr ppf ~indent:sub_expr_indent ~block_name:"Then" then_expr;
      pprint_block_expr ppf ~indent:sub_expr_indent ~block_name:"Else" else_expr
  | Match (_, var_name, pattern_exprs) ->
      print_expr "Match";
      Fmt.pf ppf "%sMatch Var: %s@." sub_expr_indent
        (Var_name.to_string var_name);
      pprint_pattern_exprs ppf ~indent:sub_expr_indent pattern_exprs
  | UnOp (_, unary_op, expr) ->
      pprint_unary_op ppf ~indent unary_op;
      pprint_expr ppf ~indent:sub_expr_indent expr
  | BinaryOp (_, binary_op, left_expr, right_expr) ->
      pprint_binary_op ppf ~indent binary_op;
      Fmt.pf ppf "%sLeftExpr@." sub_expr_indent;
      pprint_expr ppf ~indent:sub_expr_indent left_expr;
      Fmt.pf ppf "%sRightExpr@." sub_expr_indent;
      pprint_expr ppf ~indent:sub_expr_indent right_expr
  | Drop (_, dropped_var_name) ->
      print_expr (Fmt.str "Drop - %s" (Var_name.to_string dropped_var_name))
  | Free (_, freed_reuse_credit_size) ->
      print_expr "Free";
      pprint_value ppf ~indent:sub_expr_indent freed_reuse_credit_size

and pprint_block_expr ppf ~indent ~block_name (Block (_, exprs)) =
  let sub_expr_indent = indent_tab ^ indent in
  Fmt.pf ppf "%s%s Block@." indent block_name;
  List.iter (pprint_expr ppf ~indent:sub_expr_indent) exprs

and pprint_pattern_exprs ppf ~indent = function
  | [] ->
      print_string "PELEME";
      raise
        (Invalid_argument
           "Match expressions should have at least one pattern matching")
  | pattern_exprs ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter
        (fun (MPattern (_, matched_expr, block_expr)) ->
          Fmt.pf ppf "%sPatternExpr@." indent;
          pprint_matched_expr ppf ~indent:sub_expr_indent matched_expr;
          pprint_block_expr ppf ~indent:sub_expr_indent
            ~block_name:"PatternBlockExpr" block_expr)
        pattern_exprs

and pprint_matched_expr ppf ~indent matched_expr =
  let print_matched_expr = Fmt.pf ppf "%sMatchedExpr: %s@." indent in
  let sub_expr_indent = indent ^ indent_tab in
  match matched_expr with
  | MUnderscore _ -> print_matched_expr "Underscore"
  | MVariable (_, var_name) ->
      print_matched_expr (Fmt.str "Var - %s" (Var_name.to_string var_name))
  | MConstructor (_, constructor_name, matched_exprs) ->
      print_matched_expr
        (Fmt.str "Constructor - %s"
           (Constructor_name.to_string constructor_name));
      List.iter (pprint_matched_expr ppf ~indent:sub_expr_indent) matched_exprs

(* Pretty-printing Type Definition *)
and pprint_type_defn ppf ~indent (TType (_, type_name, type_constructors)) =
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%sType Name: %s@." indent (Type_name.to_string type_name);
  Fmt.pf ppf "%sType Constructors:@." indent;
  List.iter
    (pprint_type_constructor ppf ~indent:sub_expr_indent)
    type_constructors

and pprint_type_constructor ppf ~indent
    (TTypeConstructor (_, constructor_name, type_exprs)) =
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%sType Constructor Name: %s@." indent
    (Constructor_name.to_string constructor_name);
  List.iter (pprint_type_expr ppf ~indent:sub_expr_indent) type_exprs

(* Pretty-printing Function Definition *)
and pprint_function_defn ppf ~indent
    (TFun
      ( _,
        fip,
        function_name,
        borrowed_params,
        owned_params,
        body_expr,
        return_type )) =
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%sFunction Name: %s@." indent
    (Function_name.to_string function_name);
  (match fip with
  | Some (Fip n) ->
      Fmt.pf ppf "%sFunction Type - fip(%s)@." indent (string_of_int n)
  | Some (Fbip n) ->
      Fmt.pf ppf "%sFunction Type - fbip(%s)@." indent (string_of_int n)
  | _ -> ());
  Fmt.pf ppf "%sReturn Type: %s@." indent (string_of_type return_type);
  Fmt.pf ppf "%sBorrowed Param List:@." indent;
  pprint_params ppf ~indent:sub_expr_indent borrowed_params;
  Fmt.pf ppf "%sOwned Param List:@." indent;
  pprint_params ppf ~indent:sub_expr_indent owned_params;
  pprint_block_expr ppf ~indent ~block_name:"Function Body" body_expr

(* Pretty-printing Program *)
and pprint_program ppf
    (TProg (_, type_defns, function_defns, block_expr_option)) =
  Fmt.pf ppf "Program@.";
  List.iter (pprint_type_defn ppf ~indent:indent_tab) type_defns;
  List.iter (pprint_function_defn ppf ~indent:indent_tab) function_defns;
  match block_expr_option with
  | None -> ()
  | Some block_expr ->
      pprint_block_expr ppf ~indent:indent_tab ~block_name:"Main" block_expr
