open Ast.Ast_types
open Borrowed_context
open Core
open Owned_context
open Reuse_credits

exception VariableExpected

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
  | Weak (_, b, o, r, _, _) -> (b, o, r)
  | Inst (_, b, o, r, _, _) -> (b, o, r)

let get_fip_contexts_from_pattern_expr (pattern_expr : pattern_expr) :
    BorrowedSet.t * OwnedSet.t * reuse_map_entry ReuseMap.t =
  match pattern_expr with MPattern (_, b, o, r, _, _) -> (b, o, r)

let is_value_borrowed_or_top_level_fip_function (loc : loc)
    ~(value : Typed_ast.value) ~(required_fip_type : fip)
    ~(borrowed_set : BorrowedSet.t)
    ~(functions_env : Functions_env.functions_env) :
    (Var_name.t * int) Or_error.t =
  match value with
  | Typed_ast.Variable (_, _, var_name) -> (
      match assert_in_borrowed_set ~element:var_name ~borrowed_set with
      | Ok () -> Ok (var_name, 0)
      | _ ->
          let function_name =
            Function_name.of_string (Var_name.to_string var_name)
          in
          let open Result in
          Functions_env.assert_function_has_required_fip_type loc
            required_fip_type function_name functions_env
          >>= fun () ->
          Functions_env.get_fip_function_allocation_credit loc
            function_name functions_env
          >>= fun allocation_credit -> Ok (var_name, allocation_credit))
  | _ -> Or_error.of_exn VariableExpected
