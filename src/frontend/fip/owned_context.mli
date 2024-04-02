open Ast.Ast_types
open Core

module OwnedSet : Set.S with type Elt.t = Var_name.t
(** Model the context handling owned variables as a set. *)

val extend_owned_set :
  element:Var_name.t -> owned_set:OwnedSet.t -> OwnedSet.t Or_error.t
(** Given an [element] and an owned context [owned_set], extend the context or throw error if duplicates exist. *)

val extend_owned_set_by_list :
  elements:Var_name.t list -> owned_set:OwnedSet.t -> OwnedSet.t Or_error.t
(** Given a bunch of [elements] and an owned context [owned_set], extend the context with the elements or throw error if duplicates exist. *)

val assert_in_owned_set :
  element:Var_name.t -> owned_set:OwnedSet.t -> unit Or_error.t
(** Assert an [element] is already in the [owned_context]. *)

val combine_owned_sets :
  owned_set1:OwnedSet.t -> owned_set2:OwnedSet.t -> OwnedSet.t Or_error.t
(** Performs union of the two sets, throws error if their intersection is not the empty set. *)

val remove_element_from_owned_set :
  element:Var_name.t -> owned_set:OwnedSet.t -> OwnedSet.t Or_error.t
(** Remove [element] from [owned_set], returns new set or throws error if the element was not in the set to beginning with. *)

val remove_elements_from_owned_set :
  elements:Var_name.t list -> owned_set:OwnedSet.t -> OwnedSet.t Or_error.t
(** Remove [elements] from [owned_set], returns new set or throws error if any of the elements was not in the set to beginning with. *)
