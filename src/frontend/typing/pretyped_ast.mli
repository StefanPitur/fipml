open Ast.Ast_types
open Type_infer_types

type value =
  | Unit of loc * ty
  | Integer of loc * ty * int
  | Boolean of loc * ty * bool
  | Variable of loc * ty * Var_name.t
  | Constructor of loc * ty * Constructor_name.t * value list

type expr =
  | UnboxedSingleton of loc * ty * value
  | UnboxedTuple of loc * ty * value list
  | Let of loc * ty * Var_name.t list * expr * expr
  | FunApp of loc * ty * Var_name.t * value list
  | FunCall of loc * ty * Function_name.t * value list
  | If of loc * ty * expr * expr
  | IfElse of loc * ty * expr * expr * expr
  | Match of loc * ty * Var_name.t * pattern_expr list
  | UnOp of loc * ty * unary_op * expr
  | BinaryOp of loc * ty * binary_op * expr * expr
  | Drop of loc * ty * Var_name.t * expr
  | Free of loc * ty * int * expr
  | Weak of loc * ty * int * expr
  | Inst of loc * ty * int * expr

and pattern_expr = MPattern of loc * ty * matched_expr * expr

and matched_expr =
  | MUnderscore of loc * ty
  | MVariable of loc * ty * Var_name.t
  | MConstructor of loc * ty * Constructor_name.t * matched_expr list
