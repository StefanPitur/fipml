open Ast.Ast_types
open Core

type value =
  | Unit of loc * type_expr
  | Integer of loc * type_expr * int
  | Boolean of loc * type_expr * bool
  | Variable of loc * type_expr * Var_name.t
  | Constructor of loc * type_expr * Constructor_name.t * value list

type expr =
  | UnboxedSingleton of loc * type_expr * value
  | UnboxedTuple of loc * type_expr * value list
  | Let of loc * type_expr * Var_name.t list * expr * expr
  | FunApp of loc * type_expr * Var_name.t * value list
  | FunCall of loc * type_expr * Function_name.t * value list
  | If of loc * type_expr * expr * expr
  | IfElse of loc * type_expr * expr * expr * expr
  | Match of loc * type_expr * Var_name.t * pattern_expr list
  | UnOp of loc * type_expr * unary_op * expr
  | BinaryOp of loc * type_expr * binary_op * expr * expr
  | Drop of loc * type_expr * Var_name.t * expr
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
      loc * type_expr * type_expr list * Type_name.t * type_constructor list

and type_constructor =
  | TTypeConstructor of loc * type_expr * Constructor_name.t * type_expr list

type function_defn =
  | TFun of
      loc * type_expr * int * fip option * Function_name.t * param list * expr

type program =
  | TProg of type_defn list * type_expr * function_defn list * expr option

let get_expr_type (expr : expr) : type_expr =
  match expr with
  | UnboxedSingleton (_, type_expr, _) -> type_expr
  | UnboxedTuple (_, type_expr, _) -> type_expr
  | Let (_, type_expr, _, _, _) -> type_expr
  | FunApp (_, type_expr, _, _) -> type_expr
  | FunCall (_, type_expr, _, _) -> type_expr
  | If (_, type_expr, _, _) -> type_expr
  | IfElse (_, type_expr, _, _, _) -> type_expr
  | Match (_, type_expr, _, _) -> type_expr
  | UnOp (_, type_expr, _, _) -> type_expr
  | BinaryOp (_, type_expr, _, _, _) -> type_expr
  | Drop (_, type_expr, _, _) -> type_expr
  | Free (_, type_expr, _, _) -> type_expr
  | Weak (_, type_expr, _, _) -> type_expr
  | Inst (_, type_expr, _, _) -> type_expr

let rec get_matched_expr_vars (matched_expr : matched_expr) : Var_name.t list =
  match matched_expr with
  | MUnderscore _ -> []
  | MVariable (_, _, var_name) -> [ var_name ]
  | MConstructor (_, _, _, matched_exprs) ->
      List.fold
        ~f:(fun acc_var_names matched_expr ->
          get_matched_expr_vars matched_expr @ acc_var_names)
        matched_exprs ~init:[]

let rec get_match_expr_reuse_credits (matched_expr : matched_expr) : int list =
  match matched_expr with
  | MConstructor (_, _, _, matched_exprs) ->
      List.length matched_exprs
      :: List.fold matched_exprs ~init:[] ~f:(fun acc matched_expr ->
             get_match_expr_reuse_credits matched_expr @ acc)
  | _ -> [ 0 ]
