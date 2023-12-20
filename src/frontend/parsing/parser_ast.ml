open Ast.Ast_types

type expr
and block_expr =
  | Block of loc (* expr list *)

(* Type Definitions *)
type type_defn = 
  | TType of loc * Type_name.t * type_constructor list

and type_constructor =
  | TTypeConstructor of loc * Constructor_name.t * type_expr list

type function_defn =
  | TFun of loc * Function_name.t * param list * block_expr

type program = 
| TProg of type_defn list * function_defn list