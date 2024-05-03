open Ast.Ast_types
open Core
open Lambda
open Pre_lambda
open Result
open Typing

let rec target_value (value : value) (ident_context : ident_context) :
    lambda Or_error.t =
  match value with
  | Unit -> Ok lambda_unit
  | Integer n -> Ok (Lconst (const_int n))
  | Boolean b -> Ok (Lconst (const_int (Bool.to_int b)))
  | Variable var ->
      Type_context_env.get_var_type ident_context var >>= fun var_ident ->
      Ok (Lmutvar var_ident)
  | Constructor (constructor_kind, constructor_tag, _, values) -> (
      let lambda_values =
        List.map values ~f:(fun value ->
            Or_error.ok_exn (target_value value ident_context))
      in
      match constructor_kind with
      | Atom -> Ok (Lconst (const_int constructor_tag))
      | NonAtom ->
          let shape = List.map values ~f:(fun _ -> Pgenval) in
          Ok
            (Lprim
               ( Pmakeblock (constructor_tag, Mutable, Some shape),
                 lambda_values,
                 Loc_unknown )))

let rec target_expr (expr : expr) (ident_context : ident_context) :
    lambda Or_error.t =
  match expr with
  | UnboxedSingleton value -> target_value value ident_context
  | UnboxedTuple values ->
      let lambda_values =
        List.map values ~f:(fun value ->
            Or_error.ok_exn (target_value value ident_context))
      in
      let shape = List.map values ~f:(fun _ -> Pgenval) in
      Ok
        (Lprim (Pmakeblock (0, Mutable, Some shape), lambda_values, Loc_unknown))
  | Let (vars, vars_expr, expr) -> (
      match vars with
      | [ var ] ->
          let var_ident = Ident.create_local (Var_name.to_string var) in
          target_expr vars_expr ident_context >>= fun lambda_vars_expr ->
          Type_context_env.extend_typing_context ident_context var var_ident
          >>= fun extended_ident_context ->
          target_expr expr extended_ident_context >>= fun lambda_expr ->
          Ok (Llet (Strict, Pgenval, var_ident, lambda_vars_expr, lambda_expr))
      | _ ->
          target_expr vars_expr ident_context >>= fun lambda_vars_expr ->
          let extended_ident_context, vars_idents =
            List.fold_right vars ~init:(ident_context, [])
              ~f:(fun var (acc_ident_context, acc_vars_idents) ->
                let var_ident = Ident.create_local (Var_name.to_string var) in
                ( Or_error.ok_exn
                    (Type_context_env.extend_typing_context acc_ident_context
                       var var_ident),
                  var_ident :: acc_vars_idents ))
          in
          target_expr expr extended_ident_context >>= fun lambda_expr ->
          let tupled_param_ident = Ident.create_local "tupled_param" in
          let letrec_vars_detupling, _ =
            List.fold vars_idents ~init:([], 0)
              ~f:(fun
                  (acc_letrec_vars_detupling, detupling_field_index)
                  var_ident
                ->
                ( ( var_ident,
                    Lprim
                      ( Pfield (detupling_field_index, Pointer, Mutable),
                        [ Lvar tupled_param_ident ],
                        Loc_unknown ) )
                  :: acc_letrec_vars_detupling,
                  detupling_field_index + 1 ))
          in
          let let_tupled_function_lambda =
            lfunction ~kind:Tupled
              ~params:[ (tupled_param_ident, Pgenval) ]
              ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
              ~body:(Lletrec (letrec_vars_detupling, lambda_expr))
          in
          let let_tupled_function_ident = Ident.create_local "let_vars_fun" in
          Ok
            (Llet
               ( Strict,
                 Pgenval,
                 let_tupled_function_ident,
                 let_tupled_function_lambda,
                 Lapply
                   {
                     ap_func = Lvar let_tupled_function_ident;
                     ap_args = [ lambda_vars_expr ];
                     ap_loc = Loc_unknown;
                     ap_tailcall = Default_tailcall;
                     ap_inlined = Default_inline;
                     ap_specialised = Default_specialise;
                   } )))
  | FunApp (var_function, values) ->
      let lambda_values =
        List.map values ~f:(fun value ->
            Or_error.ok_exn (target_value value ident_context))
      in
      Type_context_env.get_var_type ident_context var_function
      >>= fun var_function_ident ->
      Ok
        (Lapply
           {
             ap_func = Lvar var_function_ident;
             ap_args = lambda_values;
             ap_loc = Loc_unknown;
             ap_tailcall = Default_tailcall;
             ap_inlined = Default_inline;
             ap_specialised = Default_specialise;
           })
  | FunCall (function_name, values) ->
      let function_var_name =
        Var_name.of_string (Function_name.to_string function_name)
      in
      let lambda_values =
        List.map values ~f:(fun value ->
            Or_error.ok_exn (target_value value ident_context))
      in
      Type_context_env.get_var_type ident_context function_var_name
      >>= fun function_ident ->
      Ok
        (Lapply
           {
             ap_func = Lvar function_ident;
             ap_args = lambda_values;
             ap_loc = Loc_unknown;
             ap_tailcall = Default_tailcall;
             ap_inlined = Default_inline;
             ap_specialised = Default_specialise;
           })
  | If (expr_cond, expr_then) ->
      target_expr expr_cond ident_context >>= fun lambda_expr_cond ->
      target_expr expr_then ident_context >>= fun lambda_expr_then ->
      Ok (Lifthenelse (lambda_expr_cond, lambda_expr_then, lambda_unit))
  | IfElse (expr_cond, expr_then, expr_else) ->
      target_expr expr_cond ident_context >>= fun lambda_expr_cond ->
      target_expr expr_then ident_context >>= fun lambda_expr_then ->
      target_expr expr_else ident_context >>= fun lambda_expr_else ->
      Ok (Lifthenelse (lambda_expr_cond, lambda_expr_then, lambda_expr_else))
  | Match _ -> Or_error.of_exn (Invalid_argument "not implemented")
  | UnOp (unary_op, expr) ->
      target_expr expr ident_context >>= fun lambda_expr ->
      let unary_op_primitive =
        match unary_op with UnOpNot -> Pnot | UnOpNeg -> Pnegint
      in
      Ok (Lprim (unary_op_primitive, [ lambda_expr ], Loc_unknown))
  | BinaryOp (binary_op, expr_left, expr_right) ->
      target_expr expr_left ident_context >>= fun lambda_expr_left ->
      target_expr expr_right ident_context >>= fun lambda_expr_right ->
      let binary_op_primitive =
        match binary_op with
        | BinOpPlus -> Paddint
        | BinOpMinus -> Psubint
        | BinOpMult -> Pmulint
        | BinOpDiv -> Pdivint Unsafe
        | BinOpMod -> Pmodint Unsafe
        | BinOpLt -> Pintcomp Clt
        | BinOpGt -> Pintcomp Cgt
        | BinOpLeq -> Pintcomp Cle
        | BinOpGeq -> Pintcomp Cge
        | BinOpEq -> Pintcomp Ceq
        | BinOpNeq -> Pintcomp Cne
        | BinOpAnd -> Psequand
        | BinOpOr -> Psequor
      in
      Ok
        (Lprim
           ( binary_op_primitive,
             [ lambda_expr_left; lambda_expr_right ],
             Loc_unknown ))
  | Raise -> Ok (Lstaticraise (0, []))

let target_params (function_params : Var_name.t list) :
    ((Ident.t * value_kind) list * ident_context) Or_error.t =
  Ok
    (List.fold_right function_params ~init:([], [])
       ~f:(fun function_param (acc_function_args, acc_ident_context) ->
         let ident_param =
           Ident.create_local (Var_name.to_string function_param)
         in
         ( (ident_param, Pgenval) :: acc_function_args,
           Or_error.ok_exn
             (Type_context_env.extend_typing_context acc_ident_context
                function_param ident_param) )))

let target_function_defn
    (TFun (function_name, function_params, function_body) : function_defn) :
    (Ident.t * lambda) Or_error.t =
  target_params function_params >>= fun (function_params, ident_context) ->
  target_expr function_body ident_context >>= fun lambda_function_body ->
  let function_ident =
    Ident.create_local (Function_name.to_string function_name)
  in
  Ok
    ( function_ident,
      lfunction ~kind:Curried ~params:function_params ~return:Pgenval
        ~loc:Loc_unknown ~attr:default_function_attribute
        ~body:lambda_function_body )

let target_function_defns (function_defns : function_defn list) :
    (ident_context * (Ident.t * lambda) list) Or_error.t =
  Ok
    (List.fold function_defns ~init:([], [])
       ~f:(fun
           (acc_functions_ident_context, acc_functions_ident_lambda)
           (TFun (function_name, _, _) as function_defn)
         ->
         let function_var =
           Var_name.of_string (Function_name.to_string function_name)
         in
         let function_ident, function_lambda =
           Or_error.ok_exn (target_function_defn function_defn)
         in
         ( Or_error.ok_exn
             (Type_context_env.extend_typing_context acc_functions_ident_context
                function_var function_ident),
           (function_ident, function_lambda) :: acc_functions_ident_lambda )))

let target_program (TProg (_, function_defns, main_option) : program) :
    lambda Or_error.t =
  target_function_defns function_defns
  >>= fun (functions_ident_context, functions_ident_lambda) ->
  (match main_option with
  | None -> Ok lambda_unit
  | Some main_expr -> target_expr main_expr functions_ident_context)
  >>= fun main_lambda ->
  let main_function_ident = Ident.create_local "main" in
  let main_param_ident = Ident.create_local "main_param" in
  let main_function_lambda =
    lfunction ~kind:Curried
      ~params:[ (main_param_ident, Pgenval) ]
      ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
      ~body:main_lambda
  in
  Ok
    (Lprim
       ( Psetglobal (Ident.create_persistent "Fip"),
         [
           Lletrec
             ( (main_function_ident, main_function_lambda)
               :: functions_ident_lambda
               @ Print_int_lambda.import_print_int_lambda_letrecs (),
               Lapply
                 {
                   ap_func = Lvar Print_int_lambda._print_int_ident;
                   ap_args =
                     [
                       Lapply
                         {
                           ap_func = Lvar main_function_ident;
                           ap_args = [ lambda_unit ];
                           ap_loc = Loc_unknown;
                           ap_tailcall = Default_tailcall;
                           ap_inlined = Default_inline;
                           ap_specialised = Default_specialise;
                         };
                     ];
                   ap_loc = Loc_unknown;
                   ap_tailcall = Default_tailcall;
                   ap_inlined = Default_inline;
                   ap_specialised = Default_specialise;
                 } );
         ],
         Loc_unknown ))