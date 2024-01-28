open Ast.Ast_types
open Core

exception UndefinedType of string

let custom_type_in_type_env (type_name : Type_name.t) (types_env : Type_name.t list) : unit Or_error.t = 
  let open Result in
  if List.mem types_env type_name ~equal:Type_name.(=) then 
    Ok () 
  else
    let error_string = "Type " ^ (Type_name.to_string type_name) ^ "has not been previously defined" in
    Or_error.of_exn (UndefinedType(error_string))

let custom_type_not_in_type_env (type_name : Type_name.t) (types_env : Type_name.t list) : unit Or_error.t = 
  match custom_type_in_type_env type_name types_env with
  | Ok _ -> Or_error.error_string ("Duplicate type definition for " ^ Type_name.to_string type_name)
  | Error _ -> Ok ()
