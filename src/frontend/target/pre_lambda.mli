open Ast.Ast_types
open Core
open Typing
module ConstructorTagMap : Map.S with type Key.t = Constructor_name.t

type ident_context = Ident.t Type_context_env.typing_context
type constructor_kind = Atom | NonAtom
type match_kind = Destructive | NonDestructive

type value =
  | Unit
  | Integer of int
  | Boolean of bool
  | Variable of Var_name.t
  | Constructor of constructor_kind * int * Constructor_name.t * value list

(** Note how we do not model Drop, Free, Weak or Inst, since there is no Lambda equivalent. *)
type expr =
  | UnboxedSingleton of value
  | UnboxedTuple of value list
  | Let of Var_name.t list * expr * expr
  | FunApp of Var_name.t * value list
  | FunCall of Function_name.t * value list
  | If of expr * expr
  | IfElse of expr * expr * expr
  | Match of match_kind * Var_name.t * pattern_expr list
  | UnOp of unary_op * expr
  | BinaryOp of binary_op * expr * expr
  | Raise

and pattern_expr = MPattern of matched_expr * expr

and matched_expr =
  | MUnderscore
  | MVariable of Var_name.t
  | MConstructor of Constructor_name.t * matched_expr list

type function_defn = TFun of Function_name.t * Var_name.t list * expr

type program =
  | TProg of int ConstructorTagMap.t * function_defn list * expr option

val compute_custom_constructors_tags :
  Type_defns_env.types_env ->
  Type_defns_env.constructors_env ->
  int ConstructorTagMap.t

val fresh_var : unit -> Var_name.t
val replace_underscores_with_dummy_vars : matched_expr -> matched_expr
val var_subst : Var_name.t -> Var_name.t -> expr -> expr
