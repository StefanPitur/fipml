open Ast.Ast_types

type value =
  | Unit of loc
  | Integer of loc * int
  | Boolean of loc * bool
  | Variable of loc * Var_name.t
  | Constructor of loc * Constructor_name.t * value list

type expr =
  | UnboxedSingleton of loc * value
  | UnboxedTuple of loc * value list
  | Let of loc * Var_name.t list * expr * expr
  | FunApp of loc * Var_name.t * value list
  | FunCall of loc * Function_name.t * value list
  | If of loc * expr * expr
  | IfElse of loc * expr * expr * expr
  | Match of loc * Var_name.t * pattern_expr list
  | UnOp of loc * unary_op * expr
  | BinaryOp of loc * binary_op * expr * expr
  | Drop of loc * Var_name.t * expr
  | Free of loc * int * expr
  | Weak of loc * int * expr
  | Inst of loc * int * expr

(* Match Definitions *)
and pattern_expr = MPattern of loc * matched_expr * expr

and matched_expr =
  | MUnderscore of loc
  | MVariable of loc * Var_name.t
  | MConstructor of loc * Constructor_name.t * matched_expr list

(* Type Definitions *)
type type_defn =
  | TType of loc * poly list * Type_name.t * type_constructor list

and type_constructor =
  | TTypeConstructor of loc * Constructor_name.t * type_expr list

type function_defn =
  | TFun of
      loc * int * fip option * Function_name.t * param list * expr * type_expr

type program =
  | TProg of loc * type_defn list * function_defn list * expr option
