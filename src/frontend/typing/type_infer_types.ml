open Ast.Ast_types
open Core
open Parsing
open Result

exception ListsOfDifferentLengths
exception UnableToRemoveLastElementFromEmptyList
exception PartialFunctionApplicationNotAllowed
exception FailureConvertTyToAstType
exception FailureConvertAstToTyType of string
exception FunctionExpected

(* Note: there is no Let-Polymorphism for uniqueness. *)
type ty_unique = TyVarUnique of string | TyShared | TyUnique

type ty =
  | TyVar of string
  | TyUnit
  | TyInt
  | TyBool
  | TyPoly of string list * ty
  | TyCustom of ty list * ty_unique list * ty_attr list * Type_name.t
  | TyArrow of ty_attr * ty_attr
  | TyTuple of ty_attr list

and ty_attr = ty * ty_unique

type subst = string * ty
type subst_unique = string * ty_unique
type subst_attr = string * (ty * ty_unique)
type constr = ty * ty
type constr_unique = ty_unique * ty_unique
type typing_context = ty_attr Type_context_env.typing_context

module SharingAnalysisMap = Map.Make (struct
  type t = Var_name.t

  let compare = Var_name.compare
  let sexp_of_t x = Var_name.sexp_of_t x
  let t_of_sexp x = Var_name.t_of_sexp x
end)

let rec occurs (ty_var_name : string) = function
  | TyInt | TyBool | TyUnit -> false
  | TyVar type_var_2 -> String.( = ) ty_var_name type_var_2
  | TyCustom (tys, _, ty_attrs, _) ->
      List.exists tys ~f:(fun ty -> occurs ty_var_name ty)
      || List.exists ty_attrs ~f:(fun (ty, _) -> occurs ty_var_name ty)
  | TyArrow ((ty1, _), (ty2, _)) ->
      occurs ty_var_name ty1 || occurs ty_var_name ty2
  | TyTuple tys -> List.exists tys ~f:(fun (ty, _) -> occurs ty_var_name ty)
  | TyPoly _ ->
      raise
        (Invalid_argument "did not expect poly to be in the constraints at all")

let ty_unique_subst (substs_unique : subst_unique list) (ty_unique : ty_unique)
    : ty_unique =
  match ty_unique with
  | TyShared -> TyShared
  | TyUnique -> TyUnique
  | TyVarUnique type_unique_var -> (
      match
        List.Assoc.find ~equal:String.( = ) substs_unique type_unique_var
      with
      | Some subst_ty_unique -> subst_ty_unique
      | None -> TyVarUnique type_unique_var)

let rec ty_subst (substs : subst list) (substs_unique : subst_unique list)
    (ty : ty) =
  match ty with
  | (TyInt | TyBool | TyUnit) as ty -> ty
  | TyVar type_var -> (
      match List.Assoc.find ~equal:String.( = ) substs type_var with
      | Some subst_ty -> subst_ty
      | None -> TyVar type_var)
  | TyPoly (poly_params, ty) ->
      TyPoly (poly_params, ty_subst substs substs_unique ty)
  | TyCustom (tys, ty_uniques, ty_attrs, type_name) ->
      TyCustom
        ( List.map tys ~f:(ty_subst substs substs_unique),
          List.map ty_uniques ~f:(ty_unique_subst substs_unique),
          List.map ty_attrs ~f:(ty_attr_subst substs substs_unique),
          type_name )
  | TyArrow (ty1, ty2) ->
      TyArrow
        ( ty_attr_subst substs substs_unique ty1,
          ty_attr_subst substs substs_unique ty2 )
  | TyTuple tys ->
      TyTuple (List.map tys ~f:(ty_attr_subst substs substs_unique))

and ty_attr_subst (substs : subst list) (substs_unique : subst_unique list)
    (ty_attr : ty_attr) : ty_attr =
  let ty, ty_unique = ty_attr in
  (ty_subst substs substs_unique ty, ty_unique_subst substs_unique ty_unique)

let apply_substs_unique_to_substs (substs_unique : subst_unique list)
    (substs : subst list) : subst list =
  List.map substs ~f:(fun (var, var_ty) ->
      (var, ty_subst [] substs_unique var_ty))

let ty_subst_context (typing_context : typing_context) (substs : subst list)
    (substs_unique : subst_unique list) : typing_context =
  List.map typing_context ~f:(fun (TypingContextEntry (var_name, ty_attr)) ->
      Type_context_env.TypingContextEntry
        (var_name, ty_attr_subst substs substs_unique ty_attr))

let ty_unique_equal (ty_unique1 : ty_unique) (ty_unique2 : ty_unique) : bool =
  match (ty_unique1, ty_unique2) with
  | TyShared, TyShared -> true
  | TyUnique, TyUnique -> true
  | TyVarUnique ty_var_unique1, TyVarUnique ty_var_unique2
    when String.( = ) ty_var_unique1 ty_var_unique2 ->
      true
  | _ -> false

let rec ty_equal (ty1 : ty) (ty2 : ty) : bool =
  match (ty1, ty2) with
  | TyVar ty_var1, TyVar ty_var2 -> String.( = ) ty_var1 ty_var2
  | TyUnit, TyUnit -> true
  | TyInt, TyInt -> true
  | TyBool, TyBool -> true
  | ( TyCustom
        (ty_custom_args1, ty_unique_custom_args1, ty_attr_custom_args1, type1),
      TyCustom
        (ty_custom_args2, ty_unique_custom_args2, ty_attr_custom_args2, type2) )
    ->
      Type_name.( = ) type1 type2
      && List.for_all2_exn ty_custom_args1 ty_custom_args2 ~f:ty_equal
      && List.for_all2_exn ty_unique_custom_args1 ty_unique_custom_args2
           ~f:ty_unique_equal
      && List.for_all2_exn ty_attr_custom_args1 ty_attr_custom_args2
           ~f:ty_attr_equal
  | TyArrow (ty_attr11, ty_attr12), TyArrow (ty_attr21, ty_attr22) ->
      ty_attr_equal ty_attr11 ty_attr21 && ty_attr_equal ty_attr12 ty_attr22
  | TyTuple ty_attrs1, TyTuple ty_attrs2 ->
      List.for_all2_exn ty_attrs1 ty_attrs2 ~f:ty_attr_equal
  | _ -> false

and ty_attr_equal (ty_attr1 : ty_attr) (ty_attr2 : ty_attr) : bool =
  let ty1, ty_unique1 = ty_attr1 in
  let ty2, ty_unique2 = ty_attr2 in
  ty_equal ty1 ty2 && ty_unique_equal ty_unique1 ty_unique2

let fresh =
  let index = ref 0 in
  fun () ->
    index := !index + 1;
    TyVar ("t" ^ string_of_int !index)

let fresh_unique =
  let index = ref 0 in
  fun () ->
    index := !index + 1;
    TyVarUnique ("u" ^ string_of_int !index)

let convert_ast_uniqueness_to_ty_unique (uniqueness : uniqueness)
    (uniqueness_scheme_assoc_list : subst_unique list) : ty_unique =
  match uniqueness with
  | Shared _ -> TyShared
  | Unique _ -> TyUnique
  | PolyUnique (loc, Poly (_, poly_unique)) -> (
      match
        List.Assoc.find ~equal:String.( = ) uniqueness_scheme_assoc_list
          poly_unique
      with
      | Some ty_unique -> ty_unique
      | None ->
          let error_string = string_of_loc loc in
          raise (FailureConvertAstToTyType error_string))

let rec convert_ast_typ_to_ty (typ : typ) (typ_scheme_assoc_list : subst list)
    (uniqueness_scheme_assoc_list : subst_unique list)
    (type_expr_scheme_assoc_list : (string * (ty * ty_unique)) list) : ty =
  match typ with
  | TEUnit _ -> TyUnit
  | TEInt _ -> TyInt
  | TEBool _ -> TyBool
  | TEPoly (loc, Poly (_, typ_scheme_poly_string)) -> (
      match
        List.Assoc.find ~equal:String.( = ) typ_scheme_assoc_list
          typ_scheme_poly_string
      with
      | Some ty -> ty
      | None ->
          let error_string = string_of_loc loc in
          raise (FailureConvertAstToTyType error_string))
  | TECustom
      ( _,
        custom_typ_args,
        custom_uniqueness_args,
        custom_type_expr_args,
        custom_type_name ) ->
      let ty_custom_args =
        List.map custom_typ_args ~f:(fun custom_typ_arg ->
            convert_ast_typ_to_ty custom_typ_arg typ_scheme_assoc_list
              uniqueness_scheme_assoc_list type_expr_scheme_assoc_list)
      in
      let ty_unique_args =
        List.map custom_uniqueness_args ~f:(fun custom_uniqueness_arg ->
            convert_ast_uniqueness_to_ty_unique custom_uniqueness_arg
              uniqueness_scheme_assoc_list)
      in
      let ty_attr_args =
        List.map custom_type_expr_args ~f:(fun custom_type_expr_arg ->
            convert_ast_type_to_ty_attr custom_type_expr_arg
              typ_scheme_assoc_list uniqueness_scheme_assoc_list
              type_expr_scheme_assoc_list)
      in
      TyCustom (ty_custom_args, ty_unique_args, ty_attr_args, custom_type_name)
  | TEArrow (_, input_type_expr, output_type_expr) ->
      TyArrow
        ( convert_ast_type_to_ty_attr input_type_expr typ_scheme_assoc_list
            uniqueness_scheme_assoc_list type_expr_scheme_assoc_list,
          convert_ast_type_to_ty_attr output_type_expr typ_scheme_assoc_list
            uniqueness_scheme_assoc_list type_expr_scheme_assoc_list )
  | TETuple (_, type_exprs) ->
      TyTuple
        (List.map type_exprs ~f:(fun type_expr ->
             convert_ast_type_to_ty_attr type_expr typ_scheme_assoc_list
               uniqueness_scheme_assoc_list type_expr_scheme_assoc_list))

and convert_ast_type_to_ty_attr (type_expr : type_expr)
    (typ_scheme_assoc_list : subst list)
    (uniqueness_scheme_assoc_list : subst_unique list)
    (type_expr_scheme_assoc_list : subst_attr list) : ty_attr =
  match type_expr with
  | TPoly (Poly (loc, poly)) -> (
      match
        List.Assoc.find ~equal:String.( = ) type_expr_scheme_assoc_list poly
      with
      | Some (ty, ty_unique) -> (ty, ty_unique)
      | None ->
          let error_string = string_of_loc loc in
          raise (FailureConvertAstToTyType error_string))
  | TAttr (_, typ, uniqueness) ->
      let ty =
        convert_ast_typ_to_ty typ typ_scheme_assoc_list
          uniqueness_scheme_assoc_list type_expr_scheme_assoc_list
      in
      let ty_unique =
        convert_ast_uniqueness_to_ty_unique uniqueness
          uniqueness_scheme_assoc_list
      in
      (ty, ty_unique)

let convert_ty_unique_to_ast_uniqueness (ty_unique : ty_unique) (loc : loc) :
    uniqueness Or_error.t =
  match ty_unique with
  | TyShared -> Ok (Shared loc)
  | TyUnique -> Ok (Unique loc)
  | TyVarUnique var_unique -> Ok (PolyUnique (loc, Poly (loc, var_unique)))

let rec convert_ty_to_ast_typ (ty : ty) (loc : loc) : typ Or_error.t =
  match ty with
  | TyUnit -> Ok (TEUnit loc)
  | TyInt -> Ok (TEInt loc)
  | TyBool -> Ok (TEBool loc)
  | TyCustom
      ( ty_custom_args,
        ty_unique_custom_args,
        ty_attr_custom_args,
        custom_type_name ) ->
      let custom_typ_args =
        List.map ty_custom_args ~f:(fun ty_custom_arg ->
            Or_error.ok_exn (convert_ty_to_ast_typ ty_custom_arg loc))
      in
      let custom_uniqueness_args =
        List.map ty_unique_custom_args ~f:(fun ty_unique_custom_arg ->
            Or_error.ok_exn
              (convert_ty_unique_to_ast_uniqueness ty_unique_custom_arg loc))
      in
      let custom_type_expr_args =
        List.map ty_attr_custom_args ~f:(fun ty_attr_custom_arg ->
            Or_error.ok_exn (convert_ty_attr_to_ast_type ty_attr_custom_arg loc))
      in
      Ok
        (TECustom
           ( loc,
             custom_typ_args,
             custom_uniqueness_args,
             custom_type_expr_args,
             custom_type_name ))
  | TyArrow (ty_attr1, ty_attr2) ->
      convert_ty_attr_to_ast_type ty_attr1 loc >>= fun type_expr1 ->
      convert_ty_attr_to_ast_type ty_attr2 loc >>= fun type_expr2 ->
      Ok (TEArrow (loc, type_expr1, type_expr2))
  | TyTuple ty_attrs ->
      let type_exprs =
        List.map ty_attrs ~f:(fun ty_attr ->
            Or_error.ok_exn (convert_ty_attr_to_ast_type ty_attr loc))
      in
      Ok (TETuple (loc, type_exprs))
  | TyVar ty_var -> Ok (TEPoly (loc, Poly (loc, ty_var)))
  | TyPoly _ -> Or_error.of_exn FailureConvertTyToAstType

and convert_ty_attr_to_ast_type (ty_attr : ty_attr) (loc : loc) :
    type_expr Or_error.t =
  let ty, ty_unique = ty_attr in
  convert_ty_unique_to_ast_uniqueness ty_unique loc >>= fun uniqueness ->
  convert_ty_to_ast_typ ty loc >>= fun typ -> Ok (TAttr (loc, typ, uniqueness))

let pop_last_element_from_list (lst : 'a list) : ('a * 'a list) Or_error.t =
  let reversed_lst = List.rev lst in
  match reversed_lst with
  | [] -> Or_error.of_exn UnableToRemoveLastElementFromEmptyList
  | x :: xs -> Ok (x, List.rev xs)

let get_ty_attr_function_signature ((ty, _) as ty_attr : ty_attr) :
    (ty_attr list * ty_attr) Or_error.t =
  match ty with
  | TyArrow _ ->
      let rec get_ty_attr_function_signature ((ty, _) as ty_attr : ty_attr) :
          ty_attr list =
        match ty with
        | TyArrow (in_ty_attr, out_ty_attr) ->
            in_ty_attr :: get_ty_attr_function_signature out_ty_attr
        | _ -> [ ty_attr ]
      in
      let ty_return, ty_params =
        Or_error.ok_exn
          (pop_last_element_from_list (get_ty_attr_function_signature ty_attr))
      in
      Ok (ty_params, ty_return)
  | _ -> Or_error.of_exn FunctionExpected

let get_unique_var_from_uniqueness (uniqueness : uniqueness) : string list =
  match uniqueness with
  | Shared _ | Unique _ -> []
  | PolyUnique (_, Poly (_, poly)) -> [ poly ]

let rec get_type_vars_from_type_expr (type_expr : type_expr) :
    string list * string list * string list =
  match type_expr with
  | TPoly (Poly (_, poly)) -> ([], [], [ poly ])
  | TAttr (_, typ, uniqueness) ->
      let typ_vars, uniqueness_vars, type_expr_vars =
        get_type_vars_from_typ typ
      in
      let uniqueness_var = get_unique_var_from_uniqueness uniqueness in
      (typ_vars, uniqueness_var @ uniqueness_vars, type_expr_vars)

and get_type_vars_from_typ (typ : typ) : string list * string list * string list
    =
  match typ with
  | TEUnit _ | TEInt _ | TEBool _ -> ([], [], [])
  | TEPoly (_, Poly (_, type_var_poly)) -> ([ type_var_poly ], [], [])
  | TECustom (_, typs, uniquenesses, type_exprs, _) ->
      let typs_typ_vars, typs_unique_vars, typs_type_expr_vars =
        List.fold typs ~init:([], [], [])
          ~f:(fun (acc_typ_vars, acc_unique_vars, acc_type_expr_vars) typ ->
            let typ_vars, unique_vars, type_expr_vars =
              get_type_vars_from_typ typ
            in
            ( typ_vars @ acc_typ_vars,
              unique_vars @ acc_unique_vars,
              type_expr_vars @ acc_type_expr_vars ))
      in
      let uniqueness_unique_vars =
        List.concat (List.map uniquenesses ~f:get_unique_var_from_uniqueness)
      in
      let type_exprs_typ_vars, type_exprs_unique_vars, type_exprs_type_expr_vars
          =
        List.fold type_exprs ~init:([], [], [])
          ~f:(fun
              (acc_typ_vars, acc_unique_vars, acc_type_expr_vars) type_expr ->
            let typ_vars, unique_vars, type_expr_vars =
              get_type_vars_from_type_expr type_expr
            in
            ( typ_vars @ acc_typ_vars,
              unique_vars @ acc_unique_vars,
              type_expr_vars @ acc_type_expr_vars ))
      in
      ( typs_typ_vars @ type_exprs_typ_vars,
        typs_unique_vars @ uniqueness_unique_vars @ type_exprs_unique_vars,
        typs_type_expr_vars @ type_exprs_type_expr_vars )
  | TETuple (_, type_exprs) ->
      List.fold type_exprs ~init:([], [], [])
        ~f:(fun (acc_typ_vars, acc_unique_vars, acc_type_expr_vars) type_expr ->
          let typ_vars, unique_vars, type_expr_vars =
            get_type_vars_from_type_expr type_expr
          in
          ( typ_vars @ acc_typ_vars,
            unique_vars @ acc_unique_vars,
            type_expr_vars @ acc_type_expr_vars ))
  | TEArrow (_, in_type_expr, out_type_expr) ->
      let in_typ_vars, in_uniqueness_vars, in_type_expr_vars =
        get_type_vars_from_type_expr in_type_expr
      in
      let out_typ_vars, out_uniqueness_vars, out_type_expr_vars =
        get_type_vars_from_type_expr out_type_expr
      in
      ( in_typ_vars @ out_typ_vars,
        in_uniqueness_vars @ out_uniqueness_vars,
        in_type_expr_vars @ out_type_expr_vars )

let get_type_expr_scheme_assoc_lists (type_exprs : type_expr list) :
    subst list * subst_unique list * subst_attr list =
  let typ_vars, unique_vars, type_expr_vars =
    List.fold type_exprs ~init:([], [], [])
      ~f:(fun (acc_typ_vars, acc_unique_vars, acc_type_expr_vars) type_expr ->
        let typ_vars, unique_vars, type_expr_vars =
          get_type_vars_from_type_expr type_expr
        in
        ( typ_vars @ acc_typ_vars,
          unique_vars @ acc_unique_vars,
          type_expr_vars @ acc_type_expr_vars ))
  in
  ( List.map (List.dedup_and_sort typ_vars ~compare:String.compare)
      ~f:(fun typ_var -> (typ_var, fresh ())),
    List.map (List.dedup_and_sort unique_vars ~compare:String.compare)
      ~f:(fun unique_var -> (unique_var, fresh_unique ())),
    List.map (List.dedup_and_sort type_expr_vars ~compare:String.compare)
      ~f:(fun type_expr_var -> (type_expr_var, (fresh (), fresh_unique ()))) )

let join_sharing_analysis_map (sharing_analysis_map1 : int SharingAnalysisMap.t)
    (sharing_analysis_map2 : int SharingAnalysisMap.t) :
    int SharingAnalysisMap.t =
  Map.merge_skewed sharing_analysis_map1 sharing_analysis_map2
    ~combine:(fun ~key:_ v1 v2 -> v1 + v2)

let join_max_sharing_analysis_map
    (sharing_analysis_map1 : int SharingAnalysisMap.t)
    (sharing_analysis_map2 : int SharingAnalysisMap.t) :
    int SharingAnalysisMap.t =
  Map.merge_skewed sharing_analysis_map1 sharing_analysis_map2
    ~combine:(fun ~key:_ v1 v2 -> Int.max v1 v2)

let rec get_sharing_analysis (parsed_expr : Parser_ast.expr) :
    int SharingAnalysisMap.t =
  (* TODO: if a variable x is passed where it is expected to be shared, but account for it as being used *)
  match parsed_expr with
  | UnboxedSingleton (_, value) -> get_sharing_analysis_value value
  | UnboxedTuple (_, values) -> get_sharing_analysis_values values
  | Let (_, vars, vars_expr, expr) ->
      let vars_expr_sharing_analysis_map = get_sharing_analysis vars_expr in
      let expr_sharing_analysis_map = get_sharing_analysis expr in
      let joined_sharing_analysis_map =
        join_sharing_analysis_map vars_expr_sharing_analysis_map
          expr_sharing_analysis_map
      in
      List.fold vars ~init:joined_sharing_analysis_map
        ~f:(fun acc_sharing_analysis_map var ->
          Map.remove acc_sharing_analysis_map var)
  | FunApp (_, _, values) | FunCall (_, _, values) ->
      get_sharing_analysis_values values
  | If (_, expr_cond, expr_then) ->
      join_sharing_analysis_map
        (get_sharing_analysis expr_cond)
        (get_sharing_analysis expr_then)
  | IfElse (_, expr_cond, expr_then, expr_else) ->
      join_sharing_analysis_map
        (join_max_sharing_analysis_map
           (get_sharing_analysis expr_then)
           (get_sharing_analysis expr_else))
        (get_sharing_analysis expr_cond)
  | Match (_, matched_var, pattern_exprs) ->
      let aggr_sharing_analysis_map =
        List.fold pattern_exprs ~init:SharingAnalysisMap.empty
          ~f:(fun
              acc_sharing_analysis_map
              (Parser_ast.MPattern (_, matched_expr, expr))
            ->
            let matched_vars = Parser_ast.get_matched_expr_vars matched_expr in
            let expr_sharing_analysis_map = get_sharing_analysis expr in
            let extended_sharing_analysis_map =
              join_max_sharing_analysis_map acc_sharing_analysis_map
                expr_sharing_analysis_map
            in
            List.fold matched_vars ~init:extended_sharing_analysis_map
              ~f:(fun acc_sharing_analysis_map matched_var ->
                Map.remove acc_sharing_analysis_map matched_var))
      in
      Map.update aggr_sharing_analysis_map matched_var ~f:(fun key_option ->
          match key_option with None -> 1 | Some key -> key + 1)
  | UnOp (_, _, expr) -> get_sharing_analysis expr
  | BinaryOp (_, _, expr1, expr2) ->
      join_sharing_analysis_map
        (get_sharing_analysis expr1)
        (get_sharing_analysis expr2)
  | Drop (_, drop_var, expr) ->
      Map.update (get_sharing_analysis expr) drop_var ~f:(fun key_option ->
          match key_option with None -> 1 | Some key -> key + 1)
  | Free (_, _, expr) | Weak (_, _, expr) | Inst (_, _, expr) ->
      get_sharing_analysis expr

and get_sharing_analysis_value (parsed_value : Parser_ast.value) :
    int SharingAnalysisMap.t =
  match parsed_value with
  | Unit _ | Integer _ | Boolean _ -> SharingAnalysisMap.empty
  | Variable (_, var_name) -> SharingAnalysisMap.singleton var_name 1
  | Constructor (_, _, values) -> get_sharing_analysis_values values

and get_sharing_analysis_values (parsed_values : Parser_ast.value list) :
    int SharingAnalysisMap.t =
  List.fold parsed_values ~init:SharingAnalysisMap.empty
    ~f:(fun acc_sharing_analysis_map value ->
      join_sharing_analysis_map
        (get_sharing_analysis_value value)
        acc_sharing_analysis_map)

let get_ty_unique_from_sharing_analysis
    (sharing_analysis_map : int SharingAnalysisMap.t) (var_name : Var_name.t) :
    ty_unique =
  match Map.find sharing_analysis_map var_name with
  | None -> fresh_unique ()
  | Some usage when usage = 1 -> fresh_unique ()
  | _ -> TyShared
