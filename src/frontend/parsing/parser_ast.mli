(* open Ast.Ast_types

type identifier = Variable of Var_name.t | Constructor of Constructor_name.t

type expr = 
  | Unit of loc
  | Option of loc * expr option
  | Integer of loc * int
  | Boolean of loc * bool
  | Variable of loc * Var_name.t
  | Constructor of loc * Constructor_name.t * constructor_arg list

  (* IF expr THEN expr ENDIF *)
  | If of loc * expr * expr 
  (* IF expr THEN *)
  | IfElse of loc * expr * expr * expr

  | Match of loc * identifier * match_expr list
  | DMatch of loc * identifier * match_expr list

  | BinOp of loc * binary_op * expr * expr
  | UnOp of loc * unary_op * expr

and constructor_arg = ConstructorArg of loc * expr

and block_expr = BlockExpr of loc * expr list

and match_expr = 
  | Underscore of loc
  (* TODO: continue match_expr *)


type type_defn = 
  | TType of Type_name.t * type_constructor list

and type_constructor =
  | TTypeConstructor of Constructor_name.t * type_expr list

type function_defn = 
  | TFunction of Function_name.t * param list * block_expr

type program = Prog of type_defn list * function_defn list * block_expr *)

open Ast.Ast_types

type type_defn = 
  | TType of loc * Type_name.t * type_constructor list

and type_constructor =
  | TTypeConstructor of loc * Constructor_name.t * type_expr list

type program = 
  | TProg of type_defn list