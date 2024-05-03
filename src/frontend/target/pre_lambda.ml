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

(** Note how we do not model underscores anymore as it complicates pattern-matching compilation. *)
and matched_expr =
  | MUnderscore
  | MVariable of Var_name.t
  | MConstructor of Constructor_name.t * matched_expr list

type function_defn = TFun of Function_name.t * Var_name.t list * expr

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
