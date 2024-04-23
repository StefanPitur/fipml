open Ast.Ast_types
open Parsing.Parser_ast
open Core

exception FunctionNotFound of string
exception FunctionAlreadyExists of string
exception FunctionMultipleInstancesFound
exception FipFunctionExpected of string

type function_env_entry =
  | FunctionEnvEntry of
      int * fip option * Function_name.t * type_expr list * type_expr

type functions_env = function_env_entry list

let filter_functions_env_by_name (function_name : Function_name.t)
    (functions_env : functions_env) : functions_env =
  List.filter functions_env
    ~f:(fun (FunctionEnvEntry (_, _, function_env_entry_name, _, _)) ->
      Function_name.( = ) function_name function_env_entry_name)

let assert_function_in_functions_env (loc : loc)
    (function_name : Function_name.t) (functions_env : functions_env) :
    unit Or_error.t =
  let open Result in
  let matched_functions =
    filter_functions_env_by_name function_name functions_env
  in
  match matched_functions with
  | [] ->
      let error_string =
        Fmt.str "%s. Function %s not defined" (string_of_loc loc)
          (Function_name.to_string function_name)
      in
      Or_error.of_exn (FunctionNotFound error_string)
  | _ -> Ok ()

let assert_function_not_in_functions_env (loc : loc)
    (function_name : Function_name.t) (functions_env : functions_env) :
    unit Or_error.t =
  let open Result in
  let matched_functions =
    filter_functions_env_by_name function_name functions_env
  in
  match matched_functions with
  | [] -> Ok ()
  | _ ->
      let error_string =
        Fmt.str "%s. Duplicate function %s" (string_of_loc loc)
          (Function_name.to_string function_name)
      in
      Or_error.of_exn (FunctionAlreadyExists error_string)

let get_function_by_name (loc : loc) (function_name : Function_name.t)
    (functions_env : functions_env) : function_env_entry Or_error.t =
  let matched_functions =
    filter_functions_env_by_name function_name functions_env
  in
  match matched_functions with
  | [] ->
      let error_string =
        Fmt.str "%s. Function %s not defined" (string_of_loc loc)
          (Function_name.to_string function_name)
      in
      Or_error.of_exn (FunctionNotFound error_string)
  | [ matched_function ] -> Ok matched_function
  | _ -> Or_error.of_exn FunctionMultipleInstancesFound

let get_function_params_type
    (TFun (_, _, _, _, function_params, _, _) : function_defn) : type_expr list
    =
  List.map function_params ~f:(fun (TParam (type_expr, _, _)) -> type_expr)

let get_function_mutually_recursive_group_id (loc : loc)
    (function_name : Function_name.t) (functions_env : functions_env) :
    int Or_error.t =
  let open Result in
  get_function_by_name loc function_name functions_env
  >>= fun (FunctionEnvEntry (group_id, _, _, _, _)) -> Ok group_id

let add_fip_function_defns_to_functions_env (functions_env : functions_env) :
    functions_env =
  List.fold functions_env ~init:functions_env
    ~f:(fun
        acc_functions_env
        (FunctionEnvEntry
          ( function_group_id,
            fip_option,
            function_name,
            param_type_exprs,
            return_type_expr ))
      ->
      match fip_option with
      | None -> acc_functions_env
      | _ ->
          let fiped_function_name =
            Function_name.of_string (Function_name.to_string function_name ^ "!")
          in
          FunctionEnvEntry
            ( function_group_id,
              fip_option,
              fiped_function_name,
              param_type_exprs,
              return_type_expr )
          :: acc_functions_env)

let get_mutually_recursive_function_defns_by_group_id (group_id : int)
    (function_defns : function_defn list) : functions_env =
  let mutually_recursive_functions =
    List.filter function_defns
      ~f:(fun (TFun (_, function_group_id, _, _, _, _, _)) ->
        function_group_id = group_id)
  in
  List.map mutually_recursive_functions
    ~f:(fun
        (TFun
           (_, group_id, fip_option, function_name, _, _, function_return_type)
         as function_defn)
      ->
      let function_params_type = get_function_params_type function_defn in
      FunctionEnvEntry
        ( group_id,
          fip_option,
          function_name,
          function_params_type,
          function_return_type ))

let get_mutually_recursive_functions_env_by_group_id (group_id : int)
    (functions_env : functions_env) : functions_env =
  List.filter functions_env
    ~f:(fun (FunctionEnvEntry (fun_env_entry_group_id, _, _, _, _)) ->
      Int.( = ) group_id fun_env_entry_group_id)

let get_function_signature (loc : loc) (function_name : Function_name.t)
    (functions_env : functions_env) : type_expr Or_error.t =
  let open Result in
  get_function_by_name loc function_name functions_env
  >>= fun (FunctionEnvEntry (_, _, _, param_type_exprs, return_type_expr)) ->
  Ok
    (List.fold_right param_type_exprs ~init:return_type_expr
       ~f:(fun param_type_expr acc_type_expr ->
         TAttr (loc, TEArrow (loc, param_type_expr, acc_type_expr), Shared loc)))

let get_fip_function_allocation_credit loc (function_name : Function_name.t)
    (functions_env : functions_env) : int Or_error.t =
  match get_function_by_name loc function_name functions_env with
  | Ok (FunctionEnvEntry (_, fip_option, _, _, _)) -> (
      match fip_option with
      | Some (Fip n) | Some (Fbip n) -> Ok n
      | None ->
          Or_error.of_exn
            (FipFunctionExpected (Function_name.to_string function_name)))
  | _ ->
      Or_error.of_exn (FunctionNotFound (Function_name.to_string function_name))

let assert_function_has_required_fip_type (loc : loc) (required_fip_type : fip)
    (function_name : Function_name.t) (functions_env : functions_env) :
    unit Or_error.t =
  let open Result in
  get_function_by_name loc function_name functions_env
  >>= fun (FunctionEnvEntry (_, fip_option, _, _, _)) ->
  match fip_option with
  | None ->
      let error_string =
        Fmt.str "Expected fun of fip type %s at %s@."
          (string_of_fip_option (Some required_fip_type))
          (string_of_loc loc)
      in
      raise (FipFunctionExpected error_string)
  | Some fip -> (
      match (required_fip_type, fip) with
      | Fip _, Fip _ | Fbip _, _ -> Ok ()
      | _ ->
          let error_string =
            Fmt.str "Expected fun of fip type %s at %s@."
              (string_of_fip_option (Some required_fip_type))
              (string_of_loc loc)
          in
          raise (FipFunctionExpected error_string))

let pprint_functions_env (ppf : Format.formatter)
    (functions_env : functions_env) : unit =
  List.iter functions_env
    ~f:(fun
        (FunctionEnvEntry
          ( group_id,
            fip_option,
            function_name,
            function_params_type,
            function_return_type ))
      ->
      Fmt.pf ppf "Function Name - %s@." (Function_name.to_string function_name);
      Fmt.pf ppf "Function Mutually Recursive Group Id - %i@." group_id;
      Fmt.pf ppf "Function Type - %s@." (string_of_fip_option fip_option);
      Fmt.pf ppf "Function Params Types - %s@."
        (String.concat
           (List.map function_params_type ~f:string_of_type)
           ~sep:" -> ");
      Fmt.pf ppf "Function Return Type - %s\n@."
        (string_of_type function_return_type))
