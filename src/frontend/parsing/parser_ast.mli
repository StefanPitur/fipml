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
  | FunApp of loc * Var_name.t * expr option
  | FunCall of loc * Function_name.t * expr option * expr option
  | If of loc * expr * block_expr
  | IfElse of loc * expr * block_expr * block_expr
  | Match of loc * Var_name.t * pattern_expr list
  | UnOp of loc * unary_op * expr
  | BinaryOp of loc * binary_op * expr * expr
  | Drop of loc * Var_name.t
  | Free of loc * value

(* Match Definitions *)
and pattern_expr = MPattern of loc * matched_expr * block_expr

and matched_expr =
  | MUnderscore of loc
  | MVariable of loc * Var_name.t
  | MConstructor of loc * Constructor_name.t * matched_expr list

and block_expr = Block of loc * expr list

(* Type Definitions *)
type type_defn = TType of loc * Type_name.t * type_constructor list

and type_constructor =
  | TTypeConstructor of loc * Constructor_name.t * type_expr list

type function_defn =
  | TFun of
      loc
      * fip option
      * Function_name.t
      * param list
      * param list
      * block_expr
      * type_expr

type program =
  | TProg of loc * type_defn list * function_defn list * block_expr option
