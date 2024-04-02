open Ast.Ast_types
open Owned_context
open Core

module BorrowedSet : Set.S with type Elt.t = Var_name.t
(** Model the context handling borrowed variables as a set. *)

val extend_borrowed_set :
  element:Var_name.t -> borrowed_set:BorrowedSet.t -> BorrowedSet.t Or_error.t
(** Given an [element] and a borrowed context [borrowed_set], extend the context or throw error if duplicates exist. *)

val assert_in_borrowed_set :
  element:Var_name.t -> borrowed_set:BorrowedSet.t -> unit Or_error.t
(** Assert an [element] is already in the [borrowed_context]. *)

val combine_borrowed_sets :
  borrowed_set1:BorrowedSet.t ->
  borrowed_set2:BorrowedSet.t ->
  BorrowedSet.t Or_error.t
(** Performs union of the two sets, throws error if the intersection is not the empty set. *)

val combine_owned_with_borrowed :
  owned_set:OwnedSet.t -> borrowed_set:BorrowedSet.t -> BorrowedSet.t
(** Combines the [owned_set] with the [borrowed_set] into one [OwnedSet.t], which are disjoint by the fip invariant. *)

val extend_borrowed_set_by_list :
  elements:Var_name.t list ->
  borrowed_set:BorrowedSet.t ->
  BorrowedSet.t Or_error.t
(** Given a bunch of [elements] and a borrowed context [borrowed_set], extend the context with the elements or throw error if duplicates exist. *)
