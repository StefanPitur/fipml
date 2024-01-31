(** Pretty-prints the position in lexbuf *)

val lexbuf_position_to_string: Lexing.lexbuf -> string 

val pprint_lexbuf_position: Format.formatter -> Lexing.lexbuf -> unit