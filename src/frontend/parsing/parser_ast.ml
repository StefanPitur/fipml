open Ast.Ast_types
open Core

module FreeVarSet = Set.Make (struct
  type t = Var_name.t

  let compare = Var_name.compare
  let sexp_of_t = Var_name.sexp_of_t
  let t_of_sexp = Var_name.t_of_sexp
end)

type value =
  | Unit of loc
  | Integer of loc * int
  | Boolean of loc * bool
  | Variable of loc * Var_name.t
  | Constructor of loc * Constructor_name.t * value list

type expr =
  | UnboxedSingleton of loc * value
  | UnboxedTuple of loc * value list
  | Let of loc * Var_name.t list * expr * expr
  | FunApp of loc * Var_name.t * value list
  | FunCall of loc * Function_name.t * value list
  | If of loc * expr * expr
  | IfElse of loc * expr * expr * expr
  | Match of loc * Var_name.t * pattern_expr list
  | UnOp of loc * unary_op * expr
  | BinaryOp of loc * binary_op * expr * expr
  | Drop of loc * Var_name.t * expr
  | Free of loc * int * expr
  | Weak of loc * int * expr
  | Inst of loc * int * expr

(* Match Definitions *)
and pattern_expr = MPattern of loc * matched_expr * expr

and matched_expr =
  | MUnderscore of loc
  | MVariable of loc * Var_name.t
  | MConstructor of loc * Constructor_name.t * matched_expr list

(* Type Definitions *)
type type_defn =
  | TType of
      loc
      * typ list
      * uniqueness list
      * type_expr list
      * Type_name.t
      * type_constructor list

and type_constructor =
  | TTypeConstructor of loc * Constructor_name.t * type_expr list

type function_defn =
  | TFun of
      loc * int * fip option * Function_name.t * param list * expr * type_expr

type program =
  | TProg of loc * type_defn list * function_defn list * expr option

let rec get_matched_expr_vars (matched_expr : matched_expr) : Var_name.t list =
  match matched_expr with
  | MUnderscore _ -> []
  | MVariable (_, var_name) -> [ var_name ]
  | MConstructor (_, _, matched_exprs) ->
      List.fold
        ~f:(fun acc_var_names matched_expr ->
          get_matched_expr_vars matched_expr @ acc_var_names)
        matched_exprs ~init:[]

let rec free_variables_value (value : value) : FreeVarSet.t =
  match value with
  | Unit _ | Integer _ | Boolean _ -> FreeVarSet.empty
  | Variable (_, var_name) -> FreeVarSet.singleton var_name
  | Constructor (_, _, values) ->
      List.fold values ~init:FreeVarSet.empty ~f:(fun acc_free_var_set value ->
          Set.union acc_free_var_set (free_variables_value value))

and free_variables_values (values : value list) : FreeVarSet.t =
  List.fold values ~init:FreeVarSet.empty ~f:(fun acc_free_var_set value ->
      Set.union acc_free_var_set (free_variables_value value))

and free_variables_pattern (MPattern (_, matched_expr, expr) : pattern_expr) :
    FreeVarSet.t =
  let matched_expr_free_var_set =
    FreeVarSet.of_list (get_matched_expr_vars matched_expr)
  in
  Set.diff (free_variables expr) matched_expr_free_var_set

and free_variables (expr : expr) : FreeVarSet.t =
  match expr with
  | UnboxedSingleton (_, value) -> free_variables_value value
  | UnboxedTuple (_, values) | FunCall (_, _, values) ->
      free_variables_values values
  | Let (_, var_names, var_expr, expr) ->
      let var_names_set = FreeVarSet.of_list var_names in
      Set.diff
        (Set.union (free_variables var_expr) (free_variables expr))
        var_names_set
  | FunApp (_, var_name, values) ->
      Set.add (free_variables_values values) var_name
  | If (_, cond_expr, then_expr) ->
      Set.union (free_variables cond_expr) (free_variables then_expr)
  | IfElse (_, cond_expr, then_expr, else_expr) ->
      Set.union
        (Set.union (free_variables cond_expr) (free_variables then_expr))
        (free_variables else_expr)
  | Match (_, _, patterns) ->
      let free_vars_patterns = List.map ~f:free_variables_pattern patterns in
      List.fold_left ~init:FreeVarSet.empty
        ~f:(fun acc_free_var_set set -> Set.union acc_free_var_set set)
        free_vars_patterns
  | UnOp (_, _, expr)
  | Drop (_, _, expr)
  | Free (_, _, expr)
  | Weak (_, _, expr)
  | Inst (_, _, expr) ->
      free_variables expr
  | BinaryOp (_, _, expr1, expr2) ->
      Set.union (free_variables expr1) (free_variables expr2)
