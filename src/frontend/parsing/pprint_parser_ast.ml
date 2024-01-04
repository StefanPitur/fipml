open Ast.Ast_types
open Ast.Pprint_ast_types
open Parser_ast

let indent_tab = "    "

(* Pretty-printing Expression *)
let rec pprint_expr ppf ~indent expr =
  let print_expr = Fmt.pf ppf "%sExpr: %s@." indent in
  let sub_expr_indent = indent_tab ^ indent in
  match expr with
  | Unit _ -> print_expr "Unit"
  | Integer (_, i) -> print_expr (Fmt.str "Int: %d" i)
  | Boolean (_, b) -> print_expr (Fmt.str "Bool: %b" b)
  | Option (_, o) -> (
      match o with
      | None -> print_expr (Fmt.str "Option - None")
      | Some expr -> 
        print_expr (Fmt.str "Option - Some");
        pprint_expr ppf ~indent:sub_expr_indent expr
    )
  
  | Variable (_, var_name) -> print_expr (Fmt.str "Var: %s" (Var_name.to_string var_name))
  | Constructor (_, constructor_name, constructor_args) ->
      print_expr (Fmt.str "Constructor: %s" (Constructor_name.to_string constructor_name));
      pprint_constructor_args ppf ~indent:sub_expr_indent constructor_args
  | Tuple (_, fst, snd) ->
      print_expr "Tuple";
      Fmt.pf ppf "%sFst@." sub_expr_indent;
      pprint_expr ppf ~indent:sub_expr_indent fst;
      Fmt.pf ppf "%sSnd@." sub_expr_indent;
      pprint_expr ppf ~indent:sub_expr_indent snd
  
  | Let (_, var_name, expr, in_expr) ->
      print_expr (Fmt.str "Let var: %s = " (Var_name.to_string var_name));
      pprint_expr ppf ~indent:sub_expr_indent expr;
      pprint_expr ppf ~indent:sub_expr_indent in_expr
  | FunApp (_, function_name, function_args) ->
      print_expr "FunApp";
      Fmt.pf ppf "%sFunction: %s@." sub_expr_indent (Function_name.to_string function_name);
      pprint_function_args ppf ~indent:sub_expr_indent function_args

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
      Fmt.pf ppf "%sMatch Var: %s@." sub_expr_indent (Var_name.to_string var_name);
      pprint_pattern_exprs ppf ~indent:sub_expr_indent pattern_exprs
  | DMatch (_, var_name, pattern_exprs) ->
      print_expr "DMatch";
      Fmt.pf ppf "%sDMatch Var: %s@." sub_expr_indent (Var_name.to_string var_name);
      pprint_pattern_exprs ppf ~indent:sub_expr_indent pattern_exprs

  | UnOp (_, unary_op, expr) -> 
      pprint_unary_op ppf ~indent unary_op;
      pprint_expr ppf ~indent:sub_expr_indent expr
  | BinaryOp(_, binary_op, left_expr, right_expr) -> 
      pprint_binary_op ppf ~indent binary_op;
      Fmt.pf ppf "%sLeftExpr@." sub_expr_indent;
      pprint_expr ppf ~indent:sub_expr_indent left_expr;
      Fmt.pf ppf "%sRightExpr@." sub_expr_indent;
      pprint_expr ppf ~indent:sub_expr_indent right_expr;


and pprint_constructor_args ppf ~indent = function
  | [] -> Fmt.pf ppf "%s()@." indent
  | constructor_args ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter (
        fun constructor_arg ->
          Fmt.pf ppf "%sConstructorArg@." indent;
          pprint_expr ppf ~indent:sub_expr_indent constructor_arg
      ) constructor_args

and pprint_function_args ppf ~indent = function
  | [] -> Fmt.pf ppf "%s()@." indent
  | function_args ->
      let sub_expr_indent = indent ^ indent_tab in
      List.iter (
        fun function_arg -> 
          Fmt.pf ppf "%sFunctionArg@." indent;
          pprint_expr ppf ~indent:sub_expr_indent function_arg
      ) function_args

and pprint_block_expr ppf ~indent ~block_name (Block (_, exprs)) = 
  let sub_expr_indent = indent_tab ^ indent in
  Fmt.pf ppf "%s%s Block@." indent block_name;
  List.iter (pprint_expr ppf ~indent:sub_expr_indent) exprs

and pprint_pattern_exprs ppf ~indent = function
  | [] -> raise (Invalid_argument "Match/DMatch expressions should have at least one pattern matching")
  | pattern_exprs ->
    let sub_expr_indent = indent ^ indent_tab in 
    List.iter (
      fun (MPattern(_, matched_expr, block_expr)) ->
        Fmt.pf ppf "%sPatternExpr@." indent;
        pprint_matched_expr ppf ~indent:sub_expr_indent matched_expr;
        pprint_block_expr ppf ~indent:sub_expr_indent ~block_name:"PatternBlockExpr" block_expr
    ) pattern_exprs

and pprint_matched_expr ppf ~indent matched_expr =
  let print_matched_expr = Fmt.pf ppf "%s%s@." indent in
  let sub_expr_indent = indent ^ indent_tab in
  match matched_expr with
  | MUnderscore _ -> print_matched_expr "Underscore"
  | MVariable (_, var_name) -> print_matched_expr (Fmt.str "Var - %s" (Var_name.to_string var_name))
  | MTuple(_, fst_matched_expr, snd_matched_expr) ->
      print_matched_expr "Tuple";
      pprint_matched_expr ppf ~indent:sub_expr_indent fst_matched_expr;
      pprint_matched_expr ppf ~indent:sub_expr_indent snd_matched_expr
  | MConstructor (_, constructor_name, matched_exprs) ->
      print_matched_expr (Fmt.str "Constructor - %s" (Constructor_name.to_string constructor_name));
      List.iter (pprint_matched_expr ppf ~indent:sub_expr_indent) matched_exprs
  | MOption (_, matched_expr_option) -> (
      match matched_expr_option with
      | None -> print_matched_expr "MOption - None"
      | Some matched_expr -> 
          print_matched_expr "MOption - Some";
          pprint_matched_expr ppf ~indent:sub_expr_indent matched_expr
    )

(* Pretty-printing Type Definition *)
and pprint_type_defn ppf ~indent (TType (_, type_name, type_constructors)) = 
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%sType Name: %s@." indent (Type_name.to_string type_name);
  Fmt.pf ppf "%sType Constructors:@." indent;
  List.iter (pprint_type_constructor ppf ~indent:sub_expr_indent) type_constructors

and pprint_type_constructor ppf ~indent (TTypeConstructor (_, constructor_name, type_exprs)) =
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%sType Constructor Name: %s@." indent (Constructor_name.to_string constructor_name);
  List.iter (pprint_type_expr ppf ~indent:sub_expr_indent) type_exprs

(* Pretty-printing Function Definition *)
and pprint_function_defn ppf ~indent  (TFun (_, function_name, params, body_expr)) = 
  let sub_expr_indent = indent ^ indent_tab in
  Fmt.pf ppf "%sFunction Name: %s@." indent (Function_name.to_string function_name);
  Fmt.pf ppf "%sParam List:@." indent;
  pprint_params ppf ~indent:sub_expr_indent params;
  pprint_block_expr ppf ~indent:sub_expr_indent ~block_name:"Function Body" body_expr

(* Pretty-printing Program *)
and pprint_program ppf (TProg (type_defns, function_defns, expr_option)) = 
  Fmt.pf ppf "=> TypeDefns:\n@.";
  List.iter (pprint_type_defn ppf ~indent:"") type_defns;

  Fmt.pf ppf "\n=> FunctionDefns\n@.";
  List.iter (pprint_function_defn ppf ~indent:"") function_defns;

  match expr_option with
  | None -> ()
  | Some expr -> pprint_expr ppf ~indent:"" expr
