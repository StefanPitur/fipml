open Core

(** Type-checks custom type definitions, returns if successful *)
val typecheck_type_defns : 
  Ast.Ast_types.Type_name.t list
  -> Parsing.Parser_ast.type_defn list
  -> (Typed_ast.type_defn list) Or_error.t