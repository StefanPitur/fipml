open Ast.Ast_types

type expr = 
  | Unit of loc
  | Integer of loc * int
  | Boolean of loc * bool
  | Option of loc * type_expr option * expr option

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

and block_expr = 
  | Block of loc * type_expr * expr list

and pattern_expr = 
  | MPattern of loc * matched_expr * type_expr * block_expr

(* Need to add types to matched_expr, as we do not check if the pattern to be 
   matched against are of the same type or not. 
  Could require type inference from the matched variable and unification (?)
*)
and matched_expr =
  | MUnderscore of loc * type_expr
  | MVariable of loc * type_expr * Var_name.t
  | MTuple of loc * type_expr * matched_expr * matched_expr
  | MConstructor of loc * type_expr * Constructor_name.t * matched_expr list
  | MOption of loc * type_expr option * matched_expr option


type type_defn =
  | TType of loc * type_expr * Type_name.t * type_constructor list

and type_constructor =
  | TTypeConstructor of loc * Constructor_name.t * type_expr list

type function_defn =
  | TFun of loc * Function_name.t * param list * block_expr

type program = 
| TProg of type_defn list * function_defn list * expr option
