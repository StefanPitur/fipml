open Ast.Ast_types
open Core
open Lambda
open Print_int_lambda
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

let fresh_var =
  let index = ref 0 in
  fun () ->
    index := !index + 1;
    Ident.create_local (Fmt.str "_u%i" !index)

let convert_types_env_to_constructors_tag (types_env : types_env)
    (constructors_env : constructors_env) :
    constructor_tag_map_entry ConstructorTagMap.t =
  List.fold types_env ~init:ConstructorTagMap.empty
    ~f:(fun acc_constructor_tag_map (TypesEnvEntry (_, _, _, type_name)) ->
      let type_constructors =
        List.filter constructors_env
          ~f:(fun (ConstructorEnvEntry (constructor_type, _, _)) ->
            Type_name.( = ) type_name constructor_type)
      in
      let constructor_tag_map, _, _ =
        List.fold type_constructors ~init:(acc_constructor_tag_map, 0, 0)
          ~f:(fun
              (acc_map, acc_index, acc_atom_index)
              (ConstructorEnvEntry
                (_, constructor_name, constructor_type_exprs))
            ->
            if List.length constructor_type_exprs = 0 then
              ( Map.add_exn acc_map ~key:constructor_name ~data:acc_atom_index,
                acc_index,
                acc_atom_index + 1 )
            else
              ( Map.add_exn acc_map ~key:constructor_name ~data:acc_index,
                acc_index + 1,
                acc_atom_index ))
      in
      constructor_tag_map)

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
      if List.length args = 0 then Ok (Lconst (const_int constructor_tag))
      else
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
      let lambda_values =
        List.map values ~f:(fun value ->
            Or_error.ok_exn
              (convert_typed_ast_value value ident_context constructor_tag_map))
      in
      let shape = List.map values ~f:(fun _ -> Pgenval) in
      Ok
        (Lprim (Pmakeblock (0, Mutable, Some shape), lambda_values, Loc_unknown))
  | Let (_, _, _, vars, vars_expr, expr) -> (
      match vars with
      | [ var ] ->
          let var_ident = Ident.create_local (Var_name.to_string var) in
          convert_typed_ast_expr vars_expr ident_context constructor_tag_map
          >>= fun lambda_var_expr ->
          Type_context_env.extend_typing_context ident_context var var_ident
          >>= fun ident_context ->
          convert_typed_ast_expr expr ident_context constructor_tag_map
          >>= fun lambda_expr ->
          Ok (Llet (Strict, Pgenval, var_ident, lambda_var_expr, lambda_expr))
      | _ ->
          convert_typed_ast_expr vars_expr ident_context constructor_tag_map
          >>= fun lambda_vars_expr ->
          let extended_ident_context, vars_idents =
            List.fold vars ~init:(ident_context, [])
              ~f:(fun (acc_ident_context, acc_var_idents) var ->
                let var_ident = Ident.create_local (Var_name.to_string var) in
                ( Or_error.ok_exn
                    (Type_context_env.extend_typing_context acc_ident_context
                       var var_ident),
                  acc_var_idents @ [ var_ident ] ))
          in
          convert_typed_ast_expr expr extended_ident_context constructor_tag_map
          >>= fun lambda_expr ->
          let p = Ident.create_local "p" in
          let _, letrec_args =
            List.fold vars_idents ~init:(0, [])
              ~f:(fun (field_index, acc_args) var_ident ->
                ( field_index + 1,
                  acc_args
                  @ [
                      ( var_ident,
                        Lprim
                          ( Pfield (field_index, Pointer, Mutable),
                            [ Lvar p ],
                            Loc_unknown ) );
                    ] ))
          in
          let let_function_lambda =
            lfunction ~kind:Tupled
              ~params:[ (p, Pgenval) ]
              ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
              ~body:(Lletrec (letrec_args, lambda_expr))
          in
          let let_function_ident = fresh_var () in
          Ok
            (Llet
               ( Strict,
                 Pgenval,
                 let_function_ident,
                 let_function_lambda,
                 Lapply
                   {
                     ap_func = Lvar let_function_ident;
                     ap_args = [ lambda_vars_expr ];
                     ap_loc = Loc_unknown;
                     ap_tailcall = Default_tailcall;
                     ap_inlined = Default_inline;
                     ap_specialised = Default_specialise;
                   } )))
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
  | Match (_, _, _, var, patterns) ->
      convert_typed_ast_match_expr var patterns ident_context
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

and convert_typed_ast_match_expr var _ ident_context : lambda Or_error.t =
  Type_context_env.get_var_type ident_context var >>= fun var_ident ->
  Ok
    (Lswitch
       ( Lvar var_ident,
         {
           sw_numconsts = 0;
           sw_consts = [];
           sw_numblocks = 0;
           sw_blocks = [];
           sw_failaction = Some (Lstaticraise (0, []));
         },
         Loc_unknown ))

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
    (types_env : types_env) (constructors_env : constructors_env) :
    lambda Or_error.t =
  Ok (convert_types_env_to_constructors_tag types_env constructors_env)
  >>= fun constructor_tag_map ->
  convert_typed_ast_function_defns typed_function_defns constructor_tag_map
  >>= fun (functions_ident_context, functions_ident_lambda) ->
  (match typed_main_option with
  | None -> Ok lambda_unit
  | Some main_typed_ast_expr ->
      convert_typed_ast_expr main_typed_ast_expr functions_ident_context
        ConstructorTagMap.empty)
  >>= fun main_lambda_body ->
  let main_function_ident = Ident.create_local "main" in
  let main_arg_ident = Ident.create_local "main_arg" in
  let main_function_lambda =
    lfunction ~kind:Curried
      ~params:[ (main_arg_ident, Pgenval) ]
      ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown
      ~body:main_lambda_body
  in
  Ok
    (Lprim
       ( Psetglobal (Ident.create_persistent "Fip"),
         [
           Lletrec
             ( (main_function_ident, main_function_lambda)
               :: functions_ident_lambda
               @ import_print_int_lambda_letrecs (),
               Lapply
                 {
                   ap_func = Lvar _print_int_ident;
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
