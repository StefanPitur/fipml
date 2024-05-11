open Ast.Ast_types
open Borrowed_context
open Core
open Owned_context
open Reuse_credits

type value =
  | Unit of loc * BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t
  | Integer of
      loc * BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t * int
  | Boolean of
      loc * BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t * bool
  | Variable of
      loc * BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t * Var_name.t
  | Constructor of
      loc
      * BorrowedSet.t
      * OwnedSet.t
      * reuse_map_entry ReuseMap.t
      * Constructor_name.t
      * value list

type expr =
  | UnboxedSingleton of
      loc * BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t * value
  | UnboxedTuple of
      loc * BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t * value list
  | Let of
      loc
      * BorrowedSet.t
      * OwnedSet.t
      * reuse_map_entry ReuseMap.t
      * Var_name.t list
      * expr
      * expr
  | FunApp of
      loc
      * BorrowedSet.t
      * OwnedSet.t
      * reuse_map_entry ReuseMap.t
      * Var_name.t
      * value list
  | FunCall of
      loc
      * BorrowedSet.t
      * OwnedSet.t
      * reuse_map_entry ReuseMap.t
      * Function_name.t
      * value list
  | If of
      loc
      * BorrowedSet.t
      * OwnedSet.t
      * reuse_map_entry ReuseMap.t
      * expr
      * expr
  | IfElse of
      loc
      * BorrowedSet.t
      * OwnedSet.t
      * reuse_map_entry ReuseMap.t
      * expr
      * expr
      * expr
  | Match of
      loc
      * BorrowedSet.t
      * OwnedSet.t
      * reuse_map_entry ReuseMap.t
      * Var_name.t
      * pattern_expr list
  | DMatch of
      loc
      * BorrowedSet.t
      * OwnedSet.t
      * reuse_map_entry ReuseMap.t
      * Var_name.t
      * pattern_expr list
  | UnOp of
      loc
      * BorrowedSet.t
      * OwnedSet.t
      * reuse_map_entry ReuseMap.t
      * unary_op
      * expr
  | BinaryOp of
      loc
      * BorrowedSet.t
      * OwnedSet.t
      * reuse_map_entry ReuseMap.t
      * binary_op
      * expr
      * expr
  | Drop of
      loc
      * BorrowedSet.t
      * OwnedSet.t
      * reuse_map_entry ReuseMap.t
      * Var_name.t
      * expr
  | Free of
      loc * BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t * int * expr
  | Weak of
      loc * BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t * int * expr
  | Inst of
      loc * BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t * int * expr

and pattern_expr =
  | MPattern of
      loc
      * BorrowedSet.t
      * OwnedSet.t
      * reuse_map_entry ReuseMap.t
      * Typed_ast.matched_expr
      * expr

type param = TParam of Var_name.t * borrowed option
type function_defn = TFun of loc * fip * Function_name.t * param list * expr

val get_fip_contexts_from_value :
  value -> BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t

val get_fip_contexts_from_expr :
  expr -> BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t

val get_fip_contexts_from_pattern_expr :
  pattern_expr -> BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t

val is_value_borrowed_or_top_level_fip_function :
  loc ->
  value:Typed_ast.value ->
  required_fip_type:fip ->
  borrowed_set:BorrowedSet.t ->
  functions_env:Functions_env.functions_env ->
  (Var_name.t * int) Or_error.t

val convert_params_to_fip_params : Ast.Ast_types.param list -> param list
