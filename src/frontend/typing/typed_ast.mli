open Ast.Ast_types

type expr =
  | Unit of loc * type_expr
  | Integer of loc * type_expr * int
  | Boolean of loc * type_expr * bool
  | Option of loc * type_expr * expr option
  | Variable of loc * type_expr * Var_name.t
  | Constructor of loc * type_expr * Constructor_name.t * expr list
  | Tuple of loc * type_expr * expr * expr
  | Let of loc * type_expr * Var_name.t * expr * type_expr * expr
  | FunApp of loc * type_expr * Function_name.t * expr list
  | If of loc * type_expr * expr * block_expr
  | IfElse of loc * type_expr * expr * block_expr * block_expr
  | Match of loc * type_expr * Var_name.t * pattern_expr list
  | DMatch of loc * type_expr * Var_name.t * pattern_expr list
  | UnOp of loc * type_expr * unary_op * expr
  | BinaryOp of loc * type_expr * binary_op * expr * expr

and block_expr = Block of loc * type_expr * expr list
and pattern_expr = MPattern of loc * type_expr * matched_expr * block_expr

and matched_expr =
  | MUnderscore of loc * type_expr
  | MVariable of loc * type_expr * Var_name.t
  | MTuple of loc * type_expr * matched_expr * matched_expr
  | MConstructor of loc * type_expr * Constructor_name.t * matched_expr list
  | MOption of loc * type_expr * matched_expr option

type type_defn =
  | TType of loc * type_expr * Type_name.t * type_constructor list

and type_constructor =
  | TTypeConstructor of loc * type_expr * Constructor_name.t * type_expr list

type function_defn =
  | TFun of loc * type_expr * Function_name.t * param list * block_expr

type program =
  | TProg of type_defn list * type_expr * function_defn list * block_expr option
