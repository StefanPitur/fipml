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
  | Let of loc * type_expr * type_expr list * Var_name.t list * expr * expr
  | FunApp of loc * type_expr * Var_name.t * value list
  | FunCall of loc * type_expr * Function_name.t * value list
  | If of loc * type_expr * expr * expr
  | IfElse of loc * type_expr * expr * expr * expr
  | Match of loc * type_expr * type_expr * Var_name.t * pattern_expr list
  | UnOp of loc * type_expr * unary_op * expr
  | BinaryOp of loc * type_expr * binary_op * expr * expr
  | Drop of loc * type_expr * type_expr * Var_name.t * expr
  | Free of loc * type_expr * int * expr
  | Weak of loc * type_expr * int * expr
  | Inst of loc * type_expr * int * expr

and pattern_expr = MPattern of loc * type_expr * matched_expr * expr

and matched_expr =
  | MUnderscore of loc * type_expr
  | MVariable of loc * type_expr * Var_name.t
  | MConstructor of loc * type_expr * Constructor_name.t * matched_expr list

type type_defn =
  | TType of
      loc
      * typ
      * typ list
      * uniqueness list
      * type_expr list
      * Type_name.t
      * type_constructor list

and type_constructor =
  | TTypeConstructor of loc * typ * Constructor_name.t * type_expr list

type function_defn =
  | TFun of
      loc * type_expr * int * fip option * Function_name.t * param list * expr

type program =
  | TProg of type_defn list * type_expr * function_defn list * expr option

val get_expr_type : expr -> type_expr
(** Given an [expr], return its [type_expr] *)

val get_value_loc : value -> loc

val get_matched_expr_vars_and_type_exprs :
  matched_expr -> Var_name.t list * type_expr list

val get_match_expr_reuse_credits : matched_expr -> int list
(** Given a [match_expr] return the reuse credit available by destructive match. *)

val get_mutually_recursive_typed_function_defns :
  int -> function_defn list -> function_defn list

val convert_typed_to_parser : expr -> Parsing.Parser_ast.expr
val convert_typed_to_parser_value : value -> Parsing.Parser_ast.value
val convert_typed_to_parser_values : value list -> Parsing.Parser_ast.value list

val convert_typed_to_parser_matched_expr :
  matched_expr -> Parsing.Parser_ast.matched_expr
