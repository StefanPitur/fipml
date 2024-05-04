open Ast.Ast_types
open Core
open Typing

module ConstructorTagMap = Map.Make (struct
  type t = Constructor_name.t

  let compare = Constructor_name.compare
  let sexp_of_t x = Constructor_name.sexp_of_t x
  let t_of_sexp x = Constructor_name.t_of_sexp x
end)

type ident_context = Ident.t Type_context_env.typing_context
type constructor_kind = Atom | NonAtom
type match_kind = Destructive | NonDestructive
type function_kind = Fip | NonFip

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
  | Match of match_kind * int * int * Var_name.t * pattern_expr list
  | UnOp of unary_op * expr
  | BinaryOp of binary_op * expr * expr
  | Inst of int * expr
  | Free of int * expr
  | Raise

and pattern_expr = MPattern of matched_expr * expr

(** Note how we do not model underscores anymore as it complicates pattern-matching compilation. *)
and matched_expr =
  | MUnderscore
  | MVariable of Var_name.t
  | MConstructor of Constructor_name.t * matched_expr list

type function_defn =
  | TFun of function_kind * Function_name.t * Var_name.t list * expr

type program =
  | TProg of int ConstructorTagMap.t * function_defn list * expr option

let compute_custom_constructors_tags (types_env : Type_defns_env.types_env)
    (constructors_env : Type_defns_env.constructors_env) :
    int ConstructorTagMap.t =
  List.fold types_env ~init:ConstructorTagMap.empty
    ~f:(fun acc_constructor_tag_map (TypesEnvEntry (_, _, _, type_name)) ->
      let type_constructors =
        List.filter constructors_env
          ~f:(fun (ConstructorEnvEntry (constructor_type, _, _)) ->
            Type_name.( = ) type_name constructor_type)
      in
      let constructor_tag_map, _, _ =
        List.fold type_constructors ~init:(acc_constructor_tag_map, 0, 0)
          ~f:(fun
              (acc_map, acc_index, acc_atom_index)
              (ConstructorEnvEntry
                (_, constructor_name, constructor_type_exprs))
            ->
            if List.length constructor_type_exprs = 0 then
              ( Map.add_exn acc_map ~key:constructor_name ~data:acc_atom_index,
                acc_index,
                acc_atom_index + 1 )
            else
              ( Map.add_exn acc_map ~key:constructor_name ~data:acc_index,
                acc_index + 1,
                acc_atom_index ))
      in
      constructor_tag_map)

let fresh_var =
  let index = ref 0 in
  fun () ->
    index := !index + 1;
    Var_name.of_string (Fmt.str "_t%i" !index)

let rec replace_underscores_with_dummy_vars (matched_expr : matched_expr) :
    matched_expr =
  match matched_expr with
  | MUnderscore -> MVariable (fresh_var ())
  | MVariable _ -> matched_expr
  | MConstructor (constructor_name, constructor_values) ->
      let replaced_underscored_values =
        List.map constructor_values ~f:replace_underscores_with_dummy_vars
      in
      MConstructor (constructor_name, replaced_underscored_values)

let rec var_subst_value (var : Var_name.t) (subst_var : Var_name.t)
    (value : value) : value =
  match value with
  | Unit | Integer _ | Boolean _ -> value
  | Variable variable_var ->
      if Var_name.( = ) variable_var var then Variable subst_var else value
  | Constructor
      (constructor_kind, constructor_tag, constructor_name, constructor_values)
    ->
      let constructor_subst_values =
        List.map constructor_values ~f:(var_subst_value var subst_var)
      in
      Constructor
        ( constructor_kind,
          constructor_tag,
          constructor_name,
          constructor_subst_values )

let rec var_subst (var : Var_name.t) (subst_var : Var_name.t) (expr : expr) :
    expr =
  match expr with
  | UnboxedSingleton value ->
      UnboxedSingleton (var_subst_value var subst_var value)
  | UnboxedTuple values ->
      let tuple_subst_values =
        List.map values ~f:(var_subst_value var subst_var)
      in
      UnboxedTuple tuple_subst_values
  | Let (let_vars, let_expr, expr) ->
      let subst_let_expr = var_subst var subst_var let_expr in
      let subst_expr = var_subst var subst_var expr in
      Let (let_vars, subst_let_expr, subst_expr)
  | FunApp (fun_var, values) ->
      let subst_values = List.map values ~f:(var_subst_value var subst_var) in
      let subst_fun_var =
        if Var_name.( = ) fun_var var then subst_var else fun_var
      in
      FunApp (subst_fun_var, subst_values)
  | FunCall (function_name, values) ->
      let subst_values = List.map values ~f:(var_subst_value var subst_var) in
      FunCall (function_name, subst_values)
  | If (expr_cond, expr_then) ->
      let subst_expr_cond = var_subst var subst_var expr_cond in
      let subst_expr_then = var_subst var subst_var expr_then in
      If (subst_expr_cond, subst_expr_then)
  | IfElse (expr_cond, expr_then, expr_else) ->
      let subst_expr_cond = var_subst var subst_var expr_cond in
      let subst_expr_then = var_subst var subst_var expr_then in
      let subst_expr_else = var_subst var subst_var expr_else in
      IfElse (subst_expr_cond, subst_expr_then, subst_expr_else)
  | Match (match_kind, atom_count, nonatom_count, matched_var, patterns) ->
      let subst_matched_var =
        if Var_name.( = ) matched_var var then subst_var else matched_var
      in
      let subst_patterns =
        List.map patterns ~f:(var_subst_pattern var subst_var)
      in
      Match
        ( match_kind,
          atom_count,
          nonatom_count,
          subst_matched_var,
          subst_patterns )
  | UnOp (unary_op, expr) ->
      let subst_expr = var_subst var subst_var expr in
      UnOp (unary_op, subst_expr)
  | BinaryOp (binary_op, expr_left, expr_right) ->
      let subst_expr_left = var_subst var subst_var expr_left in
      let subst_expr_right = var_subst var subst_var expr_right in
      BinaryOp (binary_op, subst_expr_left, subst_expr_right)
  | Inst (k, expr) ->
      let subst_expr = var_subst var subst_var expr in
      Inst (k, subst_expr)
  | Free (k, expr) ->
      let subst_expr = var_subst var subst_var expr in
      Free (k, subst_expr)
  | Raise -> Raise

and var_subst_pattern (var : Var_name.t) (subst_var : Var_name.t)
    (pattern : pattern_expr) : pattern_expr =
  let (MPattern (matched_expr, expr)) = pattern in
  let subst_expr = var_subst var subst_var expr in
  let subst_matched_expr = var_subst_matched_expr var subst_var matched_expr in
  MPattern (subst_matched_expr, subst_expr)

and var_subst_matched_expr (var : Var_name.t) (subst_var : Var_name.t)
    (matched_expr : matched_expr) : matched_expr =
  match matched_expr with
  | MUnderscore ->
      raise (Invalid_argument "No underscores allowed at this point")
  | MVariable matched_var ->
      let subst_matched_var =
        if Var_name.( = ) matched_var var then subst_var else matched_var
      in
      MVariable subst_matched_var
  | MConstructor (constructor_name, matched_exprs) ->
      let subst_matched_exprs =
        List.map matched_exprs ~f:(var_subst_matched_expr var subst_var)
      in
      MConstructor (constructor_name, subst_matched_exprs)

let is_atom (matched_expr : matched_expr) : bool =
  match matched_expr with
  | MConstructor (_, type_exprs) -> Int.( = ) (List.length type_exprs) 0
  | _ -> false

let split_patterns (patterns : pattern_expr list) :
    pattern_expr list * pattern_expr list =
  List.partition_tf patterns ~f:(fun (MPattern (matched_expr, _)) ->
      is_atom matched_expr)

let equal_function_kind (function_kind1 : function_kind)
    (function_kind2 : function_kind) : bool =
  match (function_kind1, function_kind2) with
  | Fip, Fip | NonFip, NonFip -> true
  | _ -> false
