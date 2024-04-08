open Parser

let pprint_lexer_token ppf token =
  Fmt.pf ppf "%s"
    (match token with
    | INT i -> Fmt.str "INT: %d@." i
    | LID lid -> Fmt.str "LID: %s@." lid
    | UID uid -> Fmt.str "UID: %s@." uid
    | LPAREN -> "("
    | RPAREN -> ")"
    | LSQPAREN -> "["
    | RSQPAREN -> "]"
    | LCURLY -> "{"
    | RCURLY -> "}"
    | COMMA -> ","
    | COLON -> ":"
    | SEMICOLON -> ";"
    | ADD -> "+"
    | SUB -> "-"
    | MUL -> "*"
    | DIV -> "/"
    | MOD -> "%%"
    | NOT -> "!"
    | BORROWED -> "^"
    | ASSIGN -> "="
    | LT -> "<"
    | GT -> ">"
    | BAR -> "|"
    | UNDERSCORE -> "_"
    | LEQ -> "<="
    | GEQ -> ">="
    | EQ -> "=="
    | NEQ -> "!="
    | AND -> "&&"
    | OR -> "||"
    | ARROW -> "->"
    | UNIT -> "()"
    | OF -> "of"
    | IF -> "if"
    | THEN -> "then"
    | ELSE -> "else"
    | ENDIF -> "endif"
    | TRUE -> "true"
    | FALSE -> "false"
    | LET -> "let"
    | FUN -> "fun"
    | IN -> "in"
    | TYPE -> "type"
    | MATCH -> "match"
    | ENDMATCH -> "endmatch"
    | WITH -> "with"
    | EOF -> "eof"
    | TYPE_INT -> "int"
    | TYPE_BOOL -> "bool"
    | TYPE_UNIT -> "unit"
    | FREE -> "free"
    | DROP -> "drop"
    | WEAK -> "weak"
    | INST -> "inst"
    | FIP -> "fip"
    | FBIP -> "fbip")
