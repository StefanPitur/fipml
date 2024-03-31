open Ast.Ast_types

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
  | FunApp of loc * type_expr * Var_name.t * expr list
  | FunCall of loc * type_expr * Function_name.t * expr list
  | If of loc * type_expr * expr * expr
  | IfElse of loc * type_expr * expr * expr * expr
  | Match of loc * type_expr * Var_name.t * pattern_expr list
  | UnOp of loc * type_expr * unary_op * expr
  | BinaryOp of loc * type_expr * binary_op * expr * expr
  | Drop of loc * type_expr * Var_name.t * expr
  | Free of loc * type_expr * value * expr

and pattern_expr = MPattern of loc * type_expr * matched_expr * expr

and matched_expr =
  | MUnderscore of loc * type_expr
  | MVariable of loc * type_expr * Var_name.t
  | MConstructor of loc * type_expr * Constructor_name.t * matched_expr list

type type_defn =
  | TType of loc * type_expr * Type_name.t * type_constructor list

and type_constructor =
  | TTypeConstructor of loc * type_expr * Constructor_name.t * type_expr list

type function_defn =
  | TFun of loc * type_expr * fip option * Function_name.t * param list * expr

type program =
  | TProg of type_defn list * type_expr * function_defn list * expr option

val get_expr_type : expr -> type_expr
(** Given an [expr], return its [type_expr] *)
