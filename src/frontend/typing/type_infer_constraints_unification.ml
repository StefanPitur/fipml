open Core
open Type_infer_types

exception UnableToUnify of string

let rec occurs (ty_var_name : string) = function
  | TyInt | TyBool | TyUnit -> false
  | TyVar type_var_2 ->  String.(=) ty_var_name type_var_2
  | TyOption ty_var -> occurs ty_var_name ty_var
  | TyCustom _ -> false
  | TyTuple (ty1, ty2) -> occurs ty_var_name ty1 || occurs ty_var_name ty2
  | TyArrow (ty1, ty2) -> occurs ty_var_name ty1 || occurs ty_var_name ty2

let rec ty_subst (substs : subst list) (ty : ty) =
  match ty with
  | TyVar type_var -> (
      match List.Assoc.find ~equal:(String.(=)) substs type_var with
      | Some subst_ty -> subst_ty
      | None -> TyVar type_var
    )
  | (TyInt | TyBool | TyUnit | TyCustom _) as ty -> ty
  | TyOption ty -> TyOption (ty_subst substs ty)
  | TyTuple (ty1, ty2)
  | TyArrow (ty1, ty2) -> TyArrow (ty_subst substs ty1, ty_subst substs ty2)

let unify (constraints : constr list) : subst list Or_error.t =
  let rec unify (constraints : constr list) (substs : subst list) =
    match constraints with
    | [] -> Ok substs
    | (t1, t2) :: constraints when ty_equal t1 t2 -> unify constraints substs
    | (TyVar type_var, t) :: constraints when not (occurs type_var t) ->
      let ts = ty_subst [(type_var, t)] in
      unify
        (List.map constraints ~f: (fun (ty1, ty2) -> (ts ty1, ts ty2)))
        ((type_var, t) :: (List.map substs ~f:(fun (ty1, ty2) -> (ty1, ts ty2))))
    | (t, TyVar type_var) :: constraints when not (occurs type_var t) ->
        let ts = ty_subst [(type_var, t)] in
        unify
          (List.map constraints ~f: (fun (ty1, ty2) -> (ts ty1, ts ty2)))
          ((type_var, t) :: (List.map substs ~f:(fun (ty1, ty2) -> (ty1, ts ty2))))
    | (TyOption ty1, TyOption ty2) :: constraints -> unify ((ty1, ty2) :: constraints) substs
    | (TyArrow (ty11, ty12), TyArrow (ty21, ty22)) :: constraints
    | (TyTuple (ty11, ty12), TyTuple (ty21, ty22)) :: constraints ->
        unify ((ty11, ty21) :: (ty12, ty22) :: constraints) substs
    | (ty1, ty2) :: _ ->
        let error_string = Fmt.str "Unable to unify types %s and %s@." in
        let string_ty1 = Pprint_type_infer.string_of_ty ty1 in
        let string_ty2 = Pprint_type_infer.string_of_ty ty2 in
        Or_error.of_exn (UnableToUnify (error_string string_ty1 string_ty2))
  in
  unify constraints []
