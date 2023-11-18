{
  open Lexing 
  open Parser

  exception LexerError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
      }
}

(** Helper regexes for the Lexer *)
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

(** Regexes for finding tokens *)
let integer_regex_expression = '-'? digit+
let id_regex_expression = (letter) (letter | digit | '_')*
let whitespace_regex_expression = [' ' '\t']+
let newline_regex_expression = '\r' | '\n' | "\r\n"


rule token = parse
  | whitespace_regex_expression { token lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "%" { MOD }
  | "~" { NOT }
  | "^" { BORROWED }
  | "=" { ASSIGN }
  | "<" { LT }
  | ">" { GT }
  | "|" { BAR }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "==" { EQ }
  | "!=" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | "->" { ARROW }
  | "()" { UNIT }
  | "of" { OF }
  | "fst" { FST }
  | "snd" { SND }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "fun" { FUN }
  | "in" { IN }
  | "begin" { BEGIN }
  | "end" { END }
  | "type" { TYPE }
  | "match" { MATCH }
  | "match!" { DMATCH }
  | "with" { WITH }
  | integer_regex_expression { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id_regex_expression { ID (Lexing.lexeme lexbuf) }
  | "/*" { multi_line_comment lexbuf }
  | "//" { single_line_comment lexbuf }
  | newline_regex_expression { next_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { raise (LexerError ("Lexer Error")) }

and multi_line_comment = parse
  | "*/" { token lexbuf }
  | newline_regex_expression { next_line lexbuf; multi_line_comment lexbuf }
  | eof { raise (LexerError("Unexpected End of FILE (EOF) - Finish comment")) }
  | _ { multi_line_comment lexbuf }

and single_line_comment = parse
  | newline_regex_expression { next_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { single_line_comment lexbuf }
