open Ast.Ast_types
open Borrowed_context
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

and pattern_expr =
  | MPattern of
      loc
      * BorrowedSet.t
      * OwnedSet.t
      * reuse_map_entry ReuseMap.t
      * Typing.Typed_ast.matched_expr
      * expr

let get_fip_contexts_from_value (value : value) :
    BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t =
  match value with
  | Unit (_, b, o, r) -> (b, o, r)
  | Integer (_, b, o, r, _) -> (b, o, r)
  | Boolean (_, b, o, r, _) -> (b, o, r)
  | Variable (_, b, o, r, _) -> (b, o, r)
  | Constructor (_, b, o, r, _, _) -> (b, o, r)

let get_fip_contexts_from_expr (expr : expr) :
    BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t =
  match expr with
  | UnboxedSingleton (_, b, o, r, _) -> (b, o, r)
  | UnboxedTuple (_, b, o, r, _) -> (b, o, r)
  | Let (_, b, o, r, _, _, _) -> (b, o, r)
  | FunApp (_, b, o, r, _, _) -> (b, o, r)
  | FunCall (_, b, o, r, _, _) -> (b, o, r)
  | If (_, b, o, r, _, _) -> (b, o, r)
  | IfElse (_, b, o, r, _, _, _) -> (b, o, r)
  | Match (_, b, o, r, _, _) -> (b, o, r)
  | DMatch (_, b, o, r, _, _) -> (b, o, r)
  | UnOp (_, b, o, r, _, _) -> (b, o, r)
  | BinaryOp (_, b, o, r, _, _, _) -> (b, o, r)
  | Drop (_, b, o, r, _, _) -> (b, o, r)
  | Free (_, b, o, r, _, _) -> (b, o, r)

let is_value_borrowed_or_top_level_function (loc : loc)
    ~(value : Typing.Typed_ast.value) ~(borrowed_set : BorrowedSet.t)
    ~(functions_env : Typing.Functions_env.functions_env) :
    (Var_name.t * int) option * bool =
  match value with
  | Typing.Typed_ast.Variable (_, _, var_name) -> (
      match assert_in_borrowed_set ~element:var_name ~borrowed_set with
      | Ok () -> (Some (var_name, 0), true)
      | _ -> (
          let function_name =
            Function_name.of_string (Var_name.to_string var_name)
          in
          match
            Typing.Functions_env.get_function_by_name loc function_name
              functions_env
          with
          | Ok (FunctionEnvEntry (_, _, _, allocation_credit)) ->
              (Some (var_name, allocation_credit), true)
          | _ -> (None, false)))
  | _ -> (None, false)
