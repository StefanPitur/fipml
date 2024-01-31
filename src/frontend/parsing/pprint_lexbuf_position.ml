let lexbuf_position_to_string (lexbuf : Lexing.lexbuf) : string =
  let pos = lexbuf.lex_curr_p in
  Fmt.str "File: %s, Line: %d, Column : %d"  pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let pprint_lexbuf_position (ppf : Format.formatter) (lexbuf : Lexing.lexbuf) : unit =
  let lexbuf_position_string = lexbuf_position_to_string lexbuf in
  Fmt.pf ppf "%s@." lexbuf_position_string
