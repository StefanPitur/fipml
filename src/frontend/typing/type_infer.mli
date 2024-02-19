open Ast.Ast_types
open Core

exception UnableToUnify

type ty =
  | TyVar of string
  | TyUnit
  | TyInt
  | TyBool
  | TyOption of ty
  | TyCustom of Type_name.t
  | TyArrow of ty * ty
  | TyTuple of ty * ty

type constr = ty * ty
type typing_context = ty Type_context_env.typing_context

val type_infer :
  Type_defns_env.types_env ->
  Type_defns_env.constructors_env ->
  Functions_env.functions_env ->
  Parsing.Parser_ast.block_expr ->
  unit Or_error.t
(** Type Inference for functions and main expression *)

val generate_constraints :
  Type_defns_env.constructors_env ->
  Functions_env.functions_env ->
  typing_context ->
  Parsing.Parser_ast.expr ->
  (typing_context * ty * constr list) Or_error.t
(** Call constraints generation on expression *)
