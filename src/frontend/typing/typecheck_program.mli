open Core

(** Given a parsed AST, perform typechecking on it and return a Typed AST if successful. *)
val typecheck_program : Parsing.Parser_ast.program -> Typed_ast.program Or_error.t