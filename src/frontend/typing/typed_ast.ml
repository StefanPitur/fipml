open Ast.Ast_types
open Core
open Parsing

type value =
  | Unit of loc * type_expr
  | Integer of loc * type_expr * int
  | Boolean of loc * type_expr * bool
  | Variable of loc * type_expr * Var_name.t
  | Constructor of loc * type_expr * Constructor_name.t * value list

type expr =
  | UnboxedSingleton of loc * type_expr * value
  | UnboxedTuple of loc * type_expr * value list
  | Let of loc * type_expr * Var_name.t list * expr * expr
  | FunApp of loc * type_expr * Var_name.t * value list
  | FunCall of loc * type_expr * Function_name.t * value list
  | If of loc * type_expr * expr * expr
  | IfElse of loc * type_expr * expr * expr * expr
  | Match of loc * type_expr * type_expr * Var_name.t * pattern_expr list
  | UnOp of loc * type_expr * unary_op * expr
  | BinaryOp of loc * type_expr * binary_op * expr * expr
  | Drop of loc * type_expr * type_expr * Var_name.t * expr
  | Free of loc * type_expr * int * expr
  | Weak of loc * type_expr * int * expr
  | Inst of loc * type_expr * int * expr

and pattern_expr = MPattern of loc * type_expr * matched_expr * expr

and matched_expr =
  | MUnderscore of loc * type_expr
  | MVariable of loc * type_expr * Var_name.t
  | MConstructor of loc * type_expr * Constructor_name.t * matched_expr list

type type_defn =
  | TType of
      loc
      * typ
      * typ list
      * uniqueness list
      * type_expr list
      * Type_name.t
      * type_constructor list

and type_constructor =
  | TTypeConstructor of loc * typ * Constructor_name.t * type_expr list

type function_defn =
  | TFun of
      loc * type_expr * int * fip option * Function_name.t * param list * expr

type program =
  | TProg of type_defn list * type_expr * function_defn list * expr option

let get_expr_type (expr : expr) : type_expr =
  match expr with
  | UnboxedSingleton (_, type_expr, _) -> type_expr
  | UnboxedTuple (_, type_expr, _) -> type_expr
  | Let (_, type_expr, _, _, _) -> type_expr
  | FunApp (_, type_expr, _, _) -> type_expr
  | FunCall (_, type_expr, _, _) -> type_expr
  | If (_, type_expr, _, _) -> type_expr
  | IfElse (_, type_expr, _, _, _) -> type_expr
  | Match (_, type_expr, _, _, _) -> type_expr
  | UnOp (_, type_expr, _, _) -> type_expr
  | BinaryOp (_, type_expr, _, _, _) -> type_expr
  | Drop (_, type_expr, _, _, _) -> type_expr
  | Free (_, type_expr, _, _) -> type_expr
  | Weak (_, type_expr, _, _) -> type_expr
  | Inst (_, type_expr, _, _) -> type_expr

let rec get_match_expr_reuse_credits (matched_expr : matched_expr) : int list =
  match matched_expr with
  | MConstructor (_, _, _, matched_exprs) ->
      List.length matched_exprs
      :: List.fold matched_exprs ~init:[] ~f:(fun acc matched_expr ->
             get_match_expr_reuse_credits matched_expr @ acc)
  | _ -> []

let get_mutually_recursive_typed_function_defns (group_id : int)
    (function_defns : function_defn list) : function_defn list =
  List.filter function_defns
    ~f:(fun (TFun (_, _, function_group_id, _, _, _, _)) ->
      Int.( = ) group_id function_group_id)

let rec convert_typed_to_parser (typed_expr : expr) : Parser_ast.expr =
  match typed_expr with
  | UnboxedSingleton (loc, _, value) ->
      let parser_value = convert_typed_to_parser_value value in
      UnboxedSingleton (loc, parser_value)
  | UnboxedTuple (loc, _, values) ->
      let parser_values = convert_typed_to_parser_values values in
      UnboxedTuple (loc, parser_values)
  | Let (loc, _, vars, vars_expr, expr) ->
      let parser_vars_expr = convert_typed_to_parser vars_expr in
      let parser_expr = convert_typed_to_parser expr in
      Let (loc, vars, parser_vars_expr, parser_expr)
  | FunApp (loc, _, var, values) ->
      let parser_values = convert_typed_to_parser_values values in
      FunApp (loc, var, parser_values)
  | FunCall (loc, _, function_name, values) ->
      let parser_values = convert_typed_to_parser_values values in
      FunCall (loc, function_name, parser_values)
  | If (loc, _, expr_cond, expr_then) ->
      let parser_expr_cond = convert_typed_to_parser expr_cond in
      let parser_expr_then = convert_typed_to_parser expr_then in
      If (loc, parser_expr_cond, parser_expr_then)
  | IfElse (loc, _, expr_cond, expr_then, expr_else) ->
      let parser_expr_cond = convert_typed_to_parser expr_cond in
      let parser_expr_then = convert_typed_to_parser expr_then in
      let parser_expr_else = convert_typed_to_parser expr_else in
      IfElse (loc, parser_expr_cond, parser_expr_then, parser_expr_else)
  | Match (loc, _, _, matched_var, pattern_exprs) ->
      Match
        ( loc,
          matched_var,
          List.map pattern_exprs
            ~f:(fun (MPattern (loc, _, matched_expr, expr)) ->
              let parser_matched_expr =
                convert_typed_to_parser_matched_expr matched_expr
              in
              let parser_expr = convert_typed_to_parser expr in
              Parser_ast.MPattern (loc, parser_matched_expr, parser_expr)) )
  | UnOp (loc, _, unary_op, expr) ->
      let parser_expr = convert_typed_to_parser expr in
      UnOp (loc, unary_op, parser_expr)
  | BinaryOp (loc, _, binary_op, expr1, expr2) ->
      let parser_expr1 = convert_typed_to_parser expr1 in
      let parser_exprs2 = convert_typed_to_parser expr2 in
      BinaryOp (loc, binary_op, parser_expr1, parser_exprs2)
  | Drop (loc, _, _, var, expr) ->
      let parser_expr = convert_typed_to_parser expr in
      Drop (loc, var, parser_expr)
  | Free (loc, _, k, expr) ->
      let parser_expr = convert_typed_to_parser expr in
      Free (loc, k, parser_expr)
  | Weak (loc, _, k, expr) ->
      let parser_expr = convert_typed_to_parser expr in
      Weak (loc, k, parser_expr)
  | Inst (loc, _, k, expr) ->
      let parser_expr = convert_typed_to_parser expr in
      Inst (loc, k, parser_expr)

and convert_typed_to_parser_value (typed_value : value) : Parser_ast.value =
  match typed_value with
  | Unit (loc, _) -> Unit loc
  | Integer (loc, _, i) -> Integer (loc, i)
  | Boolean (loc, _, b) -> Boolean (loc, b)
  | Variable (loc, _, var) -> Variable (loc, var)
  | Constructor (loc, _, constructor_name, values) ->
      Constructor (loc, constructor_name, convert_typed_to_parser_values values)

and convert_typed_to_parser_values (typed_values : value list) :
    Parser_ast.value list =
  List.map typed_values ~f:convert_typed_to_parser_value

and convert_typed_to_parser_matched_expr (typed_matched_expr : matched_expr) :
    Parser_ast.matched_expr =
  match typed_matched_expr with
  | MUnderscore (loc, _) -> MUnderscore loc
  | MVariable (loc, _, var) -> MVariable (loc, var)
  | MConstructor (loc, _, constructor_name, matched_exprs) ->
      MConstructor
        ( loc,
          constructor_name,
          List.map matched_exprs ~f:convert_typed_to_parser_matched_expr )
