(* Give better file name honestly *)

(** Entry point for calling the lexer and parser on the initial source code *)
val parse_source_code_with_error : Lexing.lexbuf -> unit
