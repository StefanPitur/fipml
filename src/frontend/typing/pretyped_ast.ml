open Ast.Ast_types
open Type_infer_types

type value =
  | Unit of loc * ty_attr
  | Integer of loc * ty_attr * int
  | Boolean of loc * ty_attr * bool
  | Variable of loc * ty_attr * Var_name.t
  | Constructor of loc * ty_attr * Constructor_name.t * value list

type expr =
  | UnboxedSingleton of loc * ty_attr * value
  | UnboxedTuple of loc * ty_attr * value list
  | Let of loc * ty_attr * ty_attr list * Var_name.t list * expr * expr
  | FunApp of loc * ty_attr * Var_name.t * value list
  | FunCall of loc * ty_attr * Function_name.t * value list
  | If of loc * ty_attr * expr * expr
  | IfElse of loc * ty_attr * expr * expr * expr
  | Match of loc * ty_attr * ty_attr * Var_name.t * pattern_expr list
  | UnOp of loc * ty_attr * unary_op * expr
  | BinaryOp of loc * ty_attr * binary_op * expr * expr
  | Drop of loc * ty_attr * ty_attr * Var_name.t * expr
  | Free of loc * ty_attr * int * expr
  | Weak of loc * ty_attr * int * expr
  | Inst of loc * ty_attr * int * expr

and pattern_expr = MPattern of loc * ty_attr * matched_expr * expr

and matched_expr =
  | MUnderscore of loc * ty_attr
  | MVariable of loc * ty_attr * Var_name.t
  | MConstructor of loc * ty_attr * Constructor_name.t * matched_expr list
