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
[@@deriving show]

type constr = ty * ty
[@@deriving show]

val type_infer :
  Type_defns_env.types_env ->
  Type_defns_env.constructors_env ->
  Parsing.Parser_ast.block_expr ->
  unit Or_error.t
(** Type Inference for functions and main expression *)
