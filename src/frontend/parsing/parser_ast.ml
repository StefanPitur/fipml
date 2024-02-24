open Ast.Ast_types

type expr =
  | Unit of loc
  | Integer of loc * int
  | Boolean of loc * bool
  | Option of loc * expr option
  | Variable of loc * Var_name.t
  | Constructor of loc * Constructor_name.t * expr list
  | Tuple of loc * expr * expr
  | Let of loc * Var_name.t * expr * expr
  | FunApp of loc * Function_name.t * expr list
  | If of loc * expr * block_expr
  | IfElse of loc * expr * block_expr * block_expr
  | Match of loc * Var_name.t * pattern_expr list
  | DMatch of loc * Var_name.t * pattern_expr list
  | UnOp of loc * unary_op * expr
  | BinaryOp of loc * binary_op * expr * expr

(* Match Definitions *)
and pattern_expr = MPattern of loc * matched_expr * block_expr

and matched_expr =
  | MUnderscore of loc
  | MVariable of loc * Var_name.t
  | MTuple of loc * matched_expr * matched_expr
  | MConstructor of loc * Constructor_name.t * matched_expr list
  | MOption of loc * matched_expr option

and block_expr = Block of loc * expr list

(* Type Definitions *)
type type_defn = TType of loc * Type_name.t * type_constructor list

and type_constructor =
  | TTypeConstructor of loc * Constructor_name.t * type_expr list

type function_defn =
  | TFun of loc * Function_name.t * param list * block_expr * type_expr

type program =
  | TProg of loc * type_defn list * function_defn list * expr option
