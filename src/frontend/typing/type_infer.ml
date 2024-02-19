open Ast.Ast_types
open Core
open Parsing.Parser_ast

exception UnableToUnify
exception ListsOfDifferentLengths

(*
  Implementation notes:
  - currently match patterns do not allow shadowing of variables
  - nothing on tuples actually works, need to implement tuples   
  - a function doesn't check that it receives the correct amount of params yet
*)

type ty =
  | TyVar of string
  | TyUnit
  | TyInt
  | TyBool
  | TyOption of ty
  | TyCustom of Type_name.t
  | TyArrow of ty * ty
  | TyTuple of ty * ty

type constr = ty * ty
type typing_context = ty Type_context_env.typing_context

let rec convert_ast_type_to_ty (type_expr : type_expr) : ty =
  match type_expr with
  | TEUnit _ -> TyUnit
  | TEInt _ -> TyInt
  | TEBool _ -> TyBool
  | TEOption (_, type_expr) -> TyOption (convert_ast_type_to_ty type_expr)
  | TECustom (_, custom_type_name) -> TyCustom custom_type_name
  | TEArrow (_, input_type_expr, output_type_expr) ->
      TyArrow
        ( convert_ast_type_to_ty input_type_expr,
          convert_ast_type_to_ty output_type_expr )

let fresh =
  let index = ref 0 in
  fun () ->
    index := !index + 1;
    TyVar ("t" ^ string_of_int !index)

let rec combine_lists (list1 : 'a list) (list2 : 'b list) :
    ('a * 'b) list Or_error.t =
  match (list1, list2) with
  | [], [] -> Ok []
  | [], _ | _, [] -> Or_error.of_exn ListsOfDifferentLengths
  | x :: xs, y :: ys ->
      let open Result in
      combine_lists xs ys >>= fun combined_list -> Ok ((x, y) :: combined_list)

let pop_last_element_from_list (lst : 'a list) : ('a * 'a list) Or_error.t =
  let reversed_lst = List.rev lst in
  match reversed_lst with
  | [] -> Or_error.error_string "Unable to pop last element from empty list"
  | x :: xs -> Ok (x, List.rev xs)

(*TODO: pass the typing context forward*)
let rec generate_constrs_block_expr
    (constructors_env : Type_defns_env.constructors_env)
    (functions_env : Functions_env.functions_env)
    (typing_context : typing_context) (Block (_, exprs) : block_expr) :
    (typing_context * ty * constr list) Or_error.t =
  let open Result in
  pop_last_element_from_list exprs >>= fun (last_expr, exprs) ->
  Ok
    (List.fold_left exprs ~init:[] ~f:(fun acc expr ->
         Or_error.ok_exn
           ( generate_constraints constructors_env functions_env typing_context
               expr
           >>= fun (_, expr_type, expr_constraints) ->
             Ok (((expr_type, TyUnit) :: expr_constraints) @ acc) )))
  >>= fun block_expr_constraints ->
  generate_constraints constructors_env functions_env typing_context last_expr
  >>= fun (_, block_type, last_expr_constraints) ->
  Ok (typing_context, block_type, last_expr_constraints @ block_expr_constraints)

and generate_constraints (constructors_env : Type_defns_env.constructors_env)
    (functions_env : Functions_env.functions_env)
    (typing_context : typing_context) (expr : expr) :
    (typing_context * ty * constr list) Or_error.t =
  let open Result in
  match expr with
  | Unit _ -> Ok (typing_context, TyUnit, [])
  | Integer _ -> Ok (typing_context, TyInt, [])
  | Boolean _ -> Ok (typing_context, TyBool, [])
  | Option (_, expr) -> (
      let t = fresh () in
      match expr with
      | None -> Ok (typing_context, t, [])
      | Some expr ->
          generate_constraints constructors_env functions_env typing_context
            expr
          >>= fun (_, expr_type, expr_constrs) ->
          Ok (typing_context, t, (t, TyOption expr_type) :: expr_constrs))
  | Variable (_, var) ->
      Type_context_env.get_var_type typing_context var >>= fun var_type ->
      Ok (typing_context, var_type, [])
  | Constructor (loc, constructor_name, constructor_exprs) ->
      let t = fresh () in
      Type_defns_env.get_constructor_by_name loc constructor_name
        constructors_env
      >>= fun (ConstructorEnvEntry
                (constructor_type, _, constructor_param_types)) ->
      combine_lists constructor_exprs constructor_param_types
      >>| List.fold_left ~init:[]
            ~f:(fun acc (constructor_expr, constructor_param_type) ->
              Or_error.ok_exn
                ( generate_constraints constructors_env functions_env
                    typing_context constructor_expr
                >>= fun (_, param_type, param_constraints) ->
                  Ok
                    ((param_type, convert_ast_type_to_ty constructor_param_type)
                     :: param_constraints
                    @ acc) ))
      >>= fun params_contraints ->
      Ok (typing_context, t, (t, TyCustom constructor_type) :: params_contraints)
  (* TODO: Implement Tuples, they're not in the language right now as a type*)
  | Tuple _ -> Ok (typing_context, TyUnit, [])
  (* Note that we do not care about Polymorphic - Let, as we only allow top-level functions *)
  | Let (_, var_name, var_expr, expr) ->
      let t = fresh () in
      generate_constraints constructors_env functions_env typing_context
        var_expr
      >>= fun (_, var_type, var_constrs) ->
      Type_context_env.extend_typing_context typing_context var_name var_type
      >>= fun extended_typing_context ->
      generate_constraints constructors_env functions_env
        extended_typing_context expr
      >>= fun (_, expr_type, expr_constrs) ->
      Ok (typing_context, t, ((t, expr_type) :: expr_constrs) @ var_constrs)
  | If (_, expr_cond, expr_then) ->
      let t = fresh () in
      generate_constraints constructors_env functions_env typing_context
        expr_cond
      >>= fun (_, expr_cond_type, expr_cond_constrs) ->
      generate_constrs_block_expr constructors_env functions_env typing_context
        expr_then
      >>= fun (_, expr_then_type, expr_then_constrs) ->
      Ok
        ( typing_context,
          t,
          [ (expr_cond_type, TyBool); (t, expr_then_type) ]
          @ expr_cond_constrs @ expr_then_constrs )
  | IfElse (_, expr_cond, expr_then, expr_else) ->
      let t = fresh () in
      generate_constraints constructors_env functions_env typing_context
        expr_cond
      >>= fun (_, expr_cond_type, expr_cond_constrs) ->
      generate_constrs_block_expr constructors_env functions_env typing_context
        expr_then
      >>= fun (_, expr_then_type, expr_then_constrs) ->
      generate_constrs_block_expr constructors_env functions_env typing_context
        expr_else
      >>= fun (_, expr_else_type, expr_else_constrs) ->
      Ok
        ( typing_context,
          t,
          [ (expr_cond_type, TyBool); (t, expr_then_type); (t, expr_else_type) ]
          @ expr_cond_constrs @ expr_then_constrs @ expr_else_constrs )
  | UnOp (_, unary_op, expr) -> (
      generate_constraints constructors_env functions_env typing_context expr
      >>= fun (_, expr_type, expr_constrs) ->
      match unary_op with
      | UnOpNeg ->
          Ok (typing_context, expr_type, (expr_type, TyInt) :: expr_constrs)
      | UnOpNot ->
          Ok (typing_context, expr_type, (expr_type, TyBool) :: expr_constrs)
      (* TODO: Implement after Tuple *)
      | UnOpFst | UnOpSnd -> Ok (typing_context, TyUnit, []))
  | BinaryOp (_, binary_op, expr1, expr2) -> (
      generate_constraints constructors_env functions_env typing_context expr1
      >>= fun (_, expr1_type, expr1_constrs) ->
      generate_constraints constructors_env functions_env typing_context expr2
      >>= fun (_, expr2_type, expr2_constrs) ->
      match binary_op with
      | BinOpPlus | BinOpMinus | BinOpMult | BinOpDiv | BinOpMod ->
          Ok
            ( typing_context,
              TyInt,
              ((expr1_type, TyInt) :: (expr2_type, TyInt) :: expr1_constrs)
              @ expr2_constrs )
      | BinOpLt | BinOpGt | BinOpLeq | BinOpGeq ->
          Ok
            ( typing_context,
              TyBool,
              ((expr1_type, TyInt) :: (expr2_type, TyInt) :: expr1_constrs)
              @ expr2_constrs )
      | BinOpEq | BinOpNeq ->
          Ok
            ( typing_context,
              TyBool,
              ((expr1_type, expr2_type) :: expr1_constrs) @ expr2_constrs )
      | BinOpAnd | BinOpOr ->
          Ok
            ( typing_context,
              TyBool,
              ((expr1_type, TyBool) :: (expr2_type, TyBool) :: expr1_constrs)
              @ expr2_constrs ))
  (*Check that arity matches with number of parameters*)
  | FunApp (loc, function_name, function_params) ->
      let open Result in
      Functions_env.get_function_by_name loc function_name functions_env
      >>= fun (_, function_args_types, function_return_type) ->
      combine_lists function_args_types function_params
      >>| List.fold_left ~init:[]
            ~f:(fun acc (function_arg_type, function_param) ->
              Or_error.ok_exn
                ( generate_constraints constructors_env functions_env
                    typing_context function_param
                >>= fun (_, function_param_type, function_param_constraints) ->
                  Ok
                    (( convert_ast_type_to_ty function_arg_type,
                       function_param_type )
                     :: function_param_constraints
                    @ acc) ))
      >>= fun function_args_constraints ->
      Ok
        ( typing_context,
          convert_ast_type_to_ty function_return_type,
          function_args_constraints )
  | Match (_, var_name, pattern_exprs) | DMatch (_, var_name, pattern_exprs) ->
      let t = fresh () in
      Type_context_env.get_var_type typing_context var_name
      >>= fun matched_var_type ->
      Ok
        (List.fold_left pattern_exprs ~init:[]
           ~f:(fun acc (MPattern (_, matched_expr, block_expr)) ->
             Or_error.ok_exn
               ( generate_constraints_matched_expr constructors_env []
                   matched_expr
               >>= fun ( matched_typing_context,
                         matched_expr_type,
                         match_expr_constraints ) ->
                 Type_context_env.union_disjoint_typing_contexts typing_context
                   matched_typing_context
                 >>= fun union_typing_contexts ->
                 generate_constrs_block_expr constructors_env functions_env
                   union_typing_contexts block_expr
                 >>= fun (_, block_expr_type, block_expr_constraints) ->
                 Ok
                   ((matched_var_type, matched_expr_type)
                    :: (t, block_expr_type) :: match_expr_constraints
                   @ block_expr_constraints @ acc) )))
      >>= fun _ -> Ok (typing_context, TyUnit, [])

and generate_constraints_matched_expr
    (constructors_env : Type_defns_env.constructors_env)
    (matched_typing_context : typing_context) (matched_expr : matched_expr) :
    (typing_context * ty * constr list) Or_error.t =
  match matched_expr with
  | MUnderscore _ ->
      let t = fresh () in
      Ok ([], t, [])
  | MVariable (_, matched_var_name) ->
      let t = fresh () in
      let open Result in
      Type_context_env.extend_typing_context matched_typing_context
        matched_var_name t
      >>= fun extended_typing_context -> Ok (extended_typing_context, t, [])
  | MTuple (_, matched_expr_fst, matched_expr_snd) ->
      let t = fresh () in
      let open Result in
      generate_constraints_matched_expr constructors_env matched_typing_context
        matched_expr_fst
      >>= fun ( matched_typing_context_fst,
                matched_expr_fst_type,
                matched_expr_fst_contraints ) ->
      generate_constraints_matched_expr constructors_env matched_typing_context
        matched_expr_snd
      >>= fun ( matched_typing_context_snd,
                matched_expr_snd_type,
                matched_expr_snd_contraints ) ->
      Type_context_env.union_disjoint_typing_contexts matched_typing_context_fst
        matched_typing_context_snd
      >>= fun union_matched_typing_context ->
      Ok
        ( union_matched_typing_context,
          t,
          (t, TyTuple (matched_expr_fst_type, matched_expr_snd_type))
          :: matched_expr_fst_contraints
          @ matched_expr_snd_contraints )
  | MConstructor (loc, constructor_name, matched_exprs) ->
      let open Result in
      Type_defns_env.get_constructor_by_name loc constructor_name
        constructors_env
      >>= fun (Type_defns_env.ConstructorEnvEntry
                (constructor_type_name, _, constructor_arg_types)) ->
      combine_lists matched_exprs constructor_arg_types
      >>= fun matched_expr_types ->
      Ok
        (List.fold_left matched_expr_types ~init:(matched_typing_context, [])
           ~f:(fun
               (acc_matched_typing_context, acc_matched_expr_constraints)
               (matched_expr, matched_expr_type)
             ->
             Or_error.ok_exn
               ( generate_constraints_matched_expr constructors_env
                   acc_matched_typing_context matched_expr
               >>= fun ( matched_typing_context,
                         matched_expr_ty,
                         matched_expr_constraints ) ->
                 Type_context_env.union_disjoint_typing_contexts
                   matched_typing_context acc_matched_typing_context
                 >>= fun union_matched_typing_context ->
                 Ok
                   ( union_matched_typing_context,
                     (matched_expr_ty, convert_ast_type_to_ty matched_expr_type)
                     :: matched_expr_constraints
                     @ acc_matched_expr_constraints ) )))
      >>= fun (matched_typing_context, matched_expr_constraints) ->
      Ok
        ( matched_typing_context,
          TyCustom constructor_type_name,
          matched_expr_constraints )
  | MOption (_, matched_expr) -> (
      let t = fresh () in
      match matched_expr with
      | None -> Ok (matched_typing_context, t, [])
      | Some matched_expr ->
          let open Result in
          generate_constraints_matched_expr constructors_env
            matched_typing_context matched_expr
          >>= fun ( matched_typing_context,
                    match_expr_type,
                    match_expr_constraints ) ->
          Ok
            ( matched_typing_context,
              t,
              (t, TyOption match_expr_type) :: match_expr_constraints ))

let type_infer (_ : Type_defns_env.types_env)
    (constructors_env : Type_defns_env.constructors_env)
    (functions_env : Functions_env.functions_env)
    (Block (_, exprs) : block_expr) : unit Or_error.t =
  match exprs with
  | [] -> Ok ()
  | expr :: _ ->
      let open Result in
      generate_constraints constructors_env functions_env [] expr >>= fun _ ->
      Ok ()
