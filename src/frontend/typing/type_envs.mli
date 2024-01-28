open Core

val custom_type_in_type_env : Ast.Ast_types.Type_name.t -> Ast.Ast_types.Type_name.t list -> unit Or_error.t;;
val custom_type_not_in_type_env : Ast.Ast_types.Type_name.t -> Ast.Ast_types.Type_name.t list -> unit Or_error.t;;