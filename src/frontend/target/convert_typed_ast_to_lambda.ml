open Ast.Ast_types
open Core
open Lambda
open Typing
open Typing.Type_defns_env
open Result

type ident_context = Ident.t Type_context_env.typing_context

module ConstructorTagMap = Map.Make (struct
  type t = Constructor_name.t

  let compare = Constructor_name.compare
  let sexp_of_t x = Constructor_name.sexp_of_t x
  let t_of_sexp x = Constructor_name.t_of_sexp x
end)

type constructor_tag_map_entry = int

let convert_types_env_to_constructors_tag (constructors_env : constructors_env)
    : constructor_tag_map_entry ConstructorTagMap.t =
  let constructor_tag_map, _ =
    List.fold constructors_env ~init:(ConstructorTagMap.empty, 1)
      ~f:(fun
          (acc_map, acc_index) (ConstructorEnvEntry (_, constructor_name, _)) ->
        ( Map.add_exn acc_map ~key:constructor_name ~data:acc_index,
          acc_index + 1 ))
  in
  constructor_tag_map

let rec convert_typed_ast_value (value : Typed_ast.value)
    (ident_context : ident_context)
    (constructor_tag_map : constructor_tag_map_entry ConstructorTagMap.t) :
    lambda Or_error.t =
  match value with
  | Unit _ -> Ok lambda_unit
  | Integer (_, _, n) -> Ok (Lconst (Const_base (Const_int n)))
  | Boolean (_, _, b) -> Ok (Lconst (Const_base (Const_int (Bool.to_int b))))
  | Variable (_, _, var) ->
      Type_context_env.get_var_type ident_context var >>= fun var_ident ->
      Ok (Lmutvar var_ident)
  | Constructor (_, _, constructor_name, args) ->
      let lambda_args =
        List.map args ~f:(fun arg ->
            Or_error.ok_exn
              (convert_typed_ast_value arg ident_context constructor_tag_map))
      in
      let constructor_tag = Map.find_exn constructor_tag_map constructor_name in
      let shape = List.map args ~f:(fun _ -> Pgenval) in
      Ok
        (Lprim
           ( Pmakeblock (constructor_tag, Mutable, Some shape),
             lambda_args,
             Loc_unknown ))

let rec convert_typed_ast_expr (expr : Typed_ast.expr)
    (ident_context : ident_context)
    (constructor_tag_map : constructor_tag_map_entry ConstructorTagMap.t) :
    lambda Or_error.t =
  match expr with
  | UnboxedSingleton (_, _, value) ->
      convert_typed_ast_value value ident_context constructor_tag_map
  | UnboxedTuple (_, _, values) ->
      (* We will make allocation for unboxed tuple at the moment *)
      let lambda_values =
        List.map values ~f:(fun value ->
            Or_error.ok_exn
              (convert_typed_ast_value value ident_context constructor_tag_map))
      in
      let shape = List.map values ~f:(fun _ -> Pgenval) in
      Ok
        (Lprim (Pmakeblock (0, Mutable, Some shape), lambda_values, Loc_unknown))
  | Let _ -> Or_error.of_exn (Invalid_argument "not implemented yet")
  | FunApp (_, _, function_var, values) ->
      let lambda_values =
        List.map values ~f:(fun value ->
            Or_error.ok_exn
              (convert_typed_ast_value value ident_context constructor_tag_map))
      in
      Type_context_env.get_var_type ident_context function_var
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
  | FunCall (_, _, function_name, values) ->
      let function_var_name =
        Var_name.of_string (Function_name.to_string function_name)
      in
      let lambda_values =
        List.map values ~f:(fun value ->
            Or_error.ok_exn
              (convert_typed_ast_value value ident_context constructor_tag_map))
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
  | If (_, _, expr_cond, expr_then) ->
      convert_typed_ast_expr expr_cond ident_context constructor_tag_map
      >>= fun lambda_expr_cond ->
      convert_typed_ast_expr expr_then ident_context constructor_tag_map
      >>= fun lambda_expr_then ->
      Ok (Lifthenelse (lambda_expr_cond, lambda_expr_then, lambda_unit))
  | IfElse (_, _, expr_cond, expr_then, expr_else) ->
      convert_typed_ast_expr expr_cond ident_context constructor_tag_map
      >>= fun lambda_expr_cond ->
      convert_typed_ast_expr expr_then ident_context constructor_tag_map
      >>= fun lambda_expr_then ->
      convert_typed_ast_expr expr_else ident_context constructor_tag_map
      >>= fun lambda_expr_else ->
      Ok (Lifthenelse (lambda_expr_cond, lambda_expr_then, lambda_expr_else))
  | Match _ -> Or_error.of_exn (Invalid_argument "not implemented yet")
  | UnOp (_, _, unary_op, expr) ->
      convert_typed_ast_expr expr ident_context constructor_tag_map
      >>= fun lambda_expr ->
      let unary_op_primitive =
        match unary_op with UnOpNot -> Pnot | UnOpNeg -> Pnegint
      in
      Ok (Lprim (unary_op_primitive, [ lambda_expr ], Loc_unknown))
  | BinaryOp (_, _, binary_op, expr_left, expr_right) ->
      convert_typed_ast_expr expr_left ident_context constructor_tag_map
      >>= fun lambda_expr_left ->
      convert_typed_ast_expr expr_right ident_context constructor_tag_map
      >>= fun lambda_expr_right ->
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
  | Drop (_, _, _, _, expr)
  | Free (_, _, _, expr)
  | Inst (_, _, _, expr)
  | Weak (_, _, _, expr) ->
      convert_typed_ast_expr expr ident_context constructor_tag_map

let convert_params (function_params : param list) :
    ((Ident.t * value_kind) list * ident_context) Or_error.t =
  Ok
    (List.fold_right function_params ~init:([], [])
       ~f:(fun (TParam (_, param_name, _)) (acc_params, acc_ident_contetx) ->
         let ident_param = Ident.create_local (Var_name.to_string param_name) in
         ( (ident_param, Pgenval) :: acc_params,
           Or_error.ok_exn
             (Type_context_env.extend_typing_context acc_ident_contetx
                param_name ident_param) )))

let convert_typed_ast_function_defn
    (TFun (_, _, _, _, function_name, function_params, function_body) :
      Typed_ast.function_defn)
    (constructor_tag_map : constructor_tag_map_entry ConstructorTagMap.t) :
    (Ident.t * lambda) Or_error.t =
  convert_params function_params >>= fun (params, ident_context) ->
  convert_typed_ast_expr function_body ident_context constructor_tag_map
  >>= fun function_body_lambda ->
  let ident_function =
    Ident.create_local (Function_name.to_string function_name)
  in
  Ok
    ( ident_function,
      lfunction ~kind:Curried ~params ~return:Pgenval ~loc:Loc_unknown
        ~attr:default_function_attribute ~body:function_body_lambda )

let convert_typed_ast_function_defns
    (typed_function_defns : Typed_ast.function_defn list)
    (constructor_tag_map : constructor_tag_map_entry ConstructorTagMap.t) :
    (ident_context * (Ident.t * lambda) list) Or_error.t =
  Ok
    (List.fold typed_function_defns ~init:([], [])
       ~f:(fun
           (acc_functions_ident_context, acc_functions_ident_lambda)
           typed_function_defn
         ->
         let (Typed_ast.TFun (_, _, _, _, function_name, _, _)) =
           typed_function_defn
         in
         let function_var =
           Var_name.of_string (Function_name.to_string function_name)
         in
         let function_ident, function_lambda =
           Or_error.ok_exn
             (convert_typed_ast_function_defn typed_function_defn
                constructor_tag_map)
         in
         ( Or_error.ok_exn
             (Type_context_env.extend_typing_context acc_functions_ident_context
                function_var function_ident),
           (function_ident, function_lambda) :: acc_functions_ident_lambda )))

let convert_typed_ast_program
    (TProg (_, _, typed_function_defns, typed_main_option) : Typed_ast.program)
    (constructors_env : constructors_env) : lambda Or_error.t =
  let constructor_tag_map =
    convert_types_env_to_constructors_tag constructors_env
  in
  convert_typed_ast_function_defns typed_function_defns constructor_tag_map
  >>= fun (functions_ident_context, functions_ident_lambda) ->
  (match typed_main_option with
  | None -> Ok lambda_unit
  | Some main_typed_ast_expr ->
      convert_typed_ast_expr main_typed_ast_expr functions_ident_context
        ConstructorTagMap.empty)
  >>= fun main_lambda ->
  let main_lambda_body =
    Lletrec
      ( functions_ident_lambda,
        Lprim
          ( Pmakeblock (0, Mutable, Some [ Pgenval ]),
            [ main_lambda ],
            Loc_unknown ) )
  in
  let main = Ident.create_local "main" in
  Ok
    (Lprim
       ( Psetglobal (Ident.create_persistent "Fip"),
         [
           Llet
             ( Strict,
               Pgenval,
               main,
               main_lambda_body,
               Lprim
                 ( Pmakeblock (0, Mutable, Some [ Pgenval ]),
                   [ Lvar main ],
                   Loc_unknown ) );
         ],
         Loc_unknown ))
