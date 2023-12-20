open Ast.Ast_types

type type_defn = 
  | TType of loc * Type_name.t * type_constructor list

and type_constructor =
  | TTypeConstructor of loc * Constructor_name.t * type_expr list

type program = 
| TProg of type_defn list