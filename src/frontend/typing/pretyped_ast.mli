open Ast.Ast_types
open Type_infer_types

type expr =
  | Unit of loc * ty
  | Integer of loc * ty * int
  | Boolean of loc * ty * bool
  | Option of loc * ty * expr option
  | Variable of loc * ty * Var_name.t
  | Constructor of loc * ty * Constructor_name.t * expr list
  | Tuple of loc * ty * expr * expr
  | Let of loc * ty * Var_name.t * expr * ty * expr
  | FunApp of loc * ty * Function_name.t * expr list
  | If of loc * ty * expr * block_expr
  | IfElse of loc * ty * expr * block_expr * block_expr
  | Match of loc * ty * Var_name.t * pattern_expr list
  | DMatch of loc * ty * Var_name.t * pattern_expr list
  | UnOp of loc * ty * unary_op * expr
  | BinaryOp of loc * ty * binary_op * expr * expr

and block_expr = Block of loc * ty * expr list
and pattern_expr = MPattern of loc * ty * matched_expr * block_expr

and matched_expr =
  | MUnderscore of loc * ty
  | MVariable of loc * ty * Var_name.t
  | MTuple of loc * ty * matched_expr * matched_expr
  | MConstructor of loc * ty * Constructor_name.t * matched_expr list
  | MOption of loc * ty * matched_expr option
