open Ast.Ast_types
open Owned_context
open Core

exception BorrowedVariableNotInContext of string
exception BorrowedVariableAlreadyInContext of string
exception BorrowedContextsNotDisjoint

module BorrowedSet = Set.Make (struct
  type t = Var_name.t

  let compare = Var_name.compare
  let sexp_of_t = Var_name.sexp_of_t
  let t_of_sexp = Var_name.t_of_sexp
end)

let extend_borrowed_set ~(element : Var_name.t) ~(borrowed_set : BorrowedSet.t)
    : BorrowedSet.t Or_error.t =
  match Set.mem borrowed_set element with
  | false -> Ok (Set.add borrowed_set element)
  | true ->
      Or_error.of_exn
        (BorrowedVariableAlreadyInContext (Var_name.to_string element))

let assert_in_borrowed_set ~(element : Var_name.t)
    ~(borrowed_set : BorrowedSet.t) : unit Or_error.t =
  match Set.mem borrowed_set element with
  | false ->
      Or_error.of_exn
        (BorrowedVariableNotInContext (Var_name.to_string element))
  | true -> Ok ()

let combine_borrowed_sets ~(borrowed_set1 : BorrowedSet.t)
    ~(borrowed_set2 : BorrowedSet.t) : BorrowedSet.t Or_error.t =
  match Set.are_disjoint borrowed_set1 borrowed_set2 with
  | false -> Or_error.of_exn BorrowedContextsNotDisjoint
  | true -> Ok (Set.union borrowed_set1 borrowed_set2)

let owned_to_borrowed ~(owned_set : OwnedSet.t) : BorrowedSet.t =
  Set.fold owned_set ~init:BorrowedSet.empty
    ~f:(fun borrowed_set_acc owned_set_entry ->
      Or_error.ok_exn
        (extend_borrowed_set ~element:owned_set_entry
           ~borrowed_set:borrowed_set_acc))

let combine_owned_with_borrowed ~(owned_set : OwnedSet.t)
    ~(borrowed_set : BorrowedSet.t) : BorrowedSet.t =
  Or_error.ok_exn
    (combine_borrowed_sets ~borrowed_set1:borrowed_set
       ~borrowed_set2:(owned_to_borrowed ~owned_set))

let extend_borrowed_set_by_list ~(elements : Var_name.t list)
    ~(borrowed_set : BorrowedSet.t) : BorrowedSet.t Or_error.t =
  let elements_set = BorrowedSet.of_list elements in
  combine_borrowed_sets ~borrowed_set1:borrowed_set ~borrowed_set2:elements_set
