open Ast.Ast_types
open Core

exception OwnedVariableAlreadyInContext of string
exception OwnedVariableNotInContext of string
exception OwnedContextsNotDisjoint
exception OwnedContextDoesntContainAllElements
exception OwnedContextsAreNotEqual

module OwnedSet = Set.Make (struct
  type t = Var_name.t

  let compare = Var_name.compare
  let sexp_of_t = Var_name.sexp_of_t
  let t_of_sexp = Var_name.t_of_sexp
end)

let extend_owned_set ~(element : Var_name.t) ~(element_type_expr : type_expr)
    ~(owned_set : OwnedSet.t) : OwnedSet.t Or_error.t =
  let open Result in
  if is_primitive element_type_expr then Ok owned_set
  else
    assert_type_expr_is_unique element_type_expr >>= fun _ ->
    match Set.mem owned_set element with
    | false -> Ok (Set.add owned_set element)
    | true ->
        Or_error.of_exn
          (OwnedVariableAlreadyInContext (Var_name.to_string element))

let assert_in_owned_set ~(element : Var_name.t) ~(owned_set : OwnedSet.t) :
    unit Or_error.t =
  match Set.mem owned_set element with
  | false ->
      Or_error.of_exn (OwnedVariableNotInContext (Var_name.to_string element))
  | true -> Ok ()

let assert_elements_not_in_owned_set ~(elements : Var_name.t list)
    ~(owned_set : OwnedSet.t) : unit Or_error.t =
  let elements_set = OwnedSet.of_list elements in
  match Set.are_disjoint elements_set owned_set with
  | true -> Ok ()
  | false -> Or_error.of_exn OwnedContextsNotDisjoint

let assert_elements_in_owned_set ~(elements : Var_name.t list)
    ~(owned_set : OwnedSet.t) : unit Or_error.t =
  let elements_set = OwnedSet.of_list elements in
  match Set.is_subset elements_set ~of_:owned_set with
  | true -> Ok ()
  | false -> Or_error.of_exn OwnedContextDoesntContainAllElements

let combine_owned_sets ~(owned_set1 : OwnedSet.t) ~(owned_set2 : OwnedSet.t) :
    OwnedSet.t Or_error.t =
  match Set.are_disjoint owned_set1 owned_set2 with
  | false -> Or_error.of_exn OwnedContextsNotDisjoint
  | true -> Ok (Set.union owned_set1 owned_set2)

let extend_owned_set_by_list ~(elements : Var_name.t list)
    ~(elements_type_exprs : type_expr list) ~(owned_set : OwnedSet.t) :
    OwnedSet.t Or_error.t =
  let non_primitive_elements =
    List.filter_map (List.zip_exn elements elements_type_exprs)
      ~f:(fun (element, element_type_expr) ->
        if is_primitive element_type_expr then None
        else (
          Or_error.ok_exn (assert_type_expr_is_unique element_type_expr);
          Some element))
  in
  let elements_set = OwnedSet.of_list non_primitive_elements in
  combine_owned_sets ~owned_set1:owned_set ~owned_set2:elements_set

let remove_element_from_owned_set ~(element : Var_name.t)
    ~(element_type_expr : type_expr) ~(owned_set : OwnedSet.t) :
    OwnedSet.t Or_error.t =
  let open Result in
  if is_primitive element_type_expr then Ok owned_set
  else
    assert_in_owned_set ~element ~owned_set >>= fun () ->
    Ok (Set.remove owned_set element)

let remove_elements_from_owned_set ~(elements : Var_name.t list)
    ~(elements_type_exprs : type_expr list) ~(owned_set : OwnedSet.t) :
    OwnedSet.t Or_error.t =
  let non_primitive_elements =
    List.filter_map (List.zip_exn elements elements_type_exprs)
      ~f:(fun (element, element_type_expr) ->
        if is_primitive element_type_expr then None else Some element)
  in
  let owned_elements_set = OwnedSet.of_list non_primitive_elements in
  if not (Set.is_subset owned_elements_set ~of_:owned_set) then
    Or_error.of_exn OwnedContextDoesntContainAllElements
  else Ok (Set.diff owned_set owned_elements_set)

let assert_owned_sets_are_equal ~(owned_set1 : OwnedSet.t)
    ~(owned_set2 : OwnedSet.t) : unit Or_error.t =
  match Set.equal owned_set1 owned_set2 with
  | true -> Ok ()
  | false -> Or_error.of_exn OwnedContextsAreNotEqual

let pprint_owned_set (ppf : Format.formatter) ~(indent : string)
    (owned_set : OwnedSet.t) : unit =
  let owned_list = Set.to_list owned_set in
  let owned_strings = List.map owned_list ~f:Var_name.to_string in
  let owned_string = String.concat ~sep:", " owned_strings in
  Fmt.pf ppf "%sOwned - [%s]@." indent owned_string
