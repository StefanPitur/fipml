open Core
open Type_infer_types

exception UnableToUnify of string

val occurs : string -> ty -> bool
(** [occurs "t1" t] returns whether or not [TyVar "t1"] appears in [t]. *)

val ty_subst : subst list -> ty -> ty
(** [ty_subst [(t1, ty1); ...; (tk, tyk)] t] replaces in type [t] type variables [ti] with [tyi] *)

val unify : constr list -> subst list Or_error.t
