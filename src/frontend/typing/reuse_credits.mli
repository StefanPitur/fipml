open Ast.Ast_types
open Core

module ReuseMap : Map.S with type Key.t = int
(** Model the context handling reuse credits as a map from reuse credit size to how many there are available *)

type reuse_map_entry = int * Var_name.t list

val allocation_credit_size : unit -> int
(** Constant function that returns a unique identifier for the key of allocation credits *)

val extend_reuse_map :
  reuse_size:int ->
  reuse_var:Var_name.t ->
  reuse_map:reuse_map_entry ReuseMap.t ->
  reuse_map_entry ReuseMap.t
(** Extends the map of reuse credits to increase the [reuse_size] availability by 1. *)

val extend_reuse_map_k_times :
  reuse_size:int ->
  reuse_var:Var_name.t ->
  k:int ->
  reuse_map:reuse_map_entry ReuseMap.t ->
  reuse_map_entry ReuseMap.t
(** Extends the map of reuse credit for [reuse_size] availability by [k]. *)

val consume_reuse_map :
  reuse_size:int ->
  reuse_map:reuse_map_entry ReuseMap.t ->
  reuse_map_entry ReuseMap.t Or_error.t
(** Consume one [reuse_size] credit by decreasing its availability by 1. *)

val combine_reuse_maps :
  reuse_map1:reuse_map_entry ReuseMap.t ->
  reuse_map2:reuse_map_entry ReuseMap.t ->
  reuse_map_entry ReuseMap.t
(** Combines two ReuseMaps into one. *)

val reuse_map_equal_fn :
  reuse_map_entry ReuseMap.t -> reuse_map_entry ReuseMap.t -> bool

val reuse_map_entry_equal_fn : reuse_map_entry -> reuse_map_entry -> bool

val assert_reuse_maps_are_equal :
  reuse_map1:reuse_map_entry ReuseMap.t ->
  reuse_map2:reuse_map_entry ReuseMap.t ->
  unit Or_error.t

val string_of_reuse_map : reuse_map_entry ReuseMap.t -> string -> string

val pprint_reuse_map :
  Format.formatter -> indent:string -> reuse_map_entry ReuseMap.t -> unit
