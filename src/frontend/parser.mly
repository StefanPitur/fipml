%{
  (* Header for auxiliary code *)
  (* TODO: Could add multiple types, such as CHAR, STRING, LIST... *)
%}

/* Tokens Definition */
%token<int> INT
%token<string> LID
%token<string> UID
%token LPAREN
%token RPAREN
%token COMMA
%token COLON
%token SEMICOLON
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token NOT
%token BORROWED
%token ASSIGN
%token LT
%token GT
%token BAR
%token UNDERSCORE
%token LEQ
%token GEQ
%token EQ
%token NEQ
%token AND
%token OR
%token ARROW
%token UNIT
%token OF
%token FST
%token SND
%token IF
%token THEN
%token ELSE
%token ENDIF
%token TRUE
%token FALSE
%token LET
%token REC
%token FUN
%token IN
%token BEGIN
%token END
%token TYPE
%token MATCH
%token DMATCH
%token ENDMATCH
%token WITH
%token SOME
%token NONE
%token EOF

/* Types Tokens */
%token TYPE_INT
%token TYPE_FLOAT
%token TYPE_CHAR
%token TYPE_BOOL
%token TYPE_STRING
%token TYPE_UNIT
%token TYPE_OPTION

/* Precedence and associativity */
%nonassoc LT GT LEQ GEQ EQ NEQ IN
%right FST SND SOME
%left OR
%left AND
%right NOT
%left ADD SUB
%left MUL DIV MOD

/* Starting non-terminal, endpoint for calling the parser */
%start <unit> program
%%

program:
| list(type_defn); list(function_defn); option(expr); EOF { print_string "Finished Parsing!\n" }


type_expr:
| LID {}
| TYPE_INT {}
| TYPE_FLOAT {}
| TYPE_CHAR {}
| TYPE_BOOL {}
| TYPE_STRING {}
| TYPE_UNIT {}
| type_expr; TYPE_OPTION {}


/* Type Definition Production Rules */
// Type Definition Structure Production Rules 
type_defn:
| TYPE; LID; ASSIGN; nonempty_list(type_constructor) {}


// Type Definition Constructors Production Rules
type_constructor:
| BAR; UID; option(type_constructor_arguments) {}


// Type Definition Constructors' Arguments Production Rules
type_constructor_arguments:
| OF; separated_nonempty_list(MUL, type_expr) {}
(* --------------------------------------------------------- *)


/* Function Definition Production Rules */
function_defn:
| FUN; option(REC); LID; list(function_param); ASSIGN; block_expr {}


function_param:
| option(BORROWED); LID {}
| LPAREN; option(BORROWED); LID; COLON; type_expr; RPAREN {}


/* Block Expression Definition Production Rules */
block_expr:
| BEGIN; separated_list(SEMICOLON, expr); END {}


expr:
/* Simple expression containing values, variables and applied constructors */
| value {}
| SOME; expr {}
| LID {}
| constructor_expr {}

/* Convoluted expressions */
| unary_op; expr {}
| expr; binary_op; expr {}
| LPAREN; expr; RPAREN {}
| LPAREN; expr; COMMA expr; RPAREN {}
| LET; LID; ASSIGN; expr; IN; expr {}

/* Control Flow - IF statements */
| IF; expr; THEN; expr; ENDIF {}
| IF; expr; THEN; expr; ELSE expr; ENDIF {}

/* Control Flow - MATCH / DMATCH statements */
| MATCH; LID; WITH; match_expr+; ENDMATCH {}
| DMATCH; LID; WITH; match_expr+; ENDMATCH {}

/* Constructor expression */
constructor_expr:
| UID {}
| UID; LPAREN; separated_nonempty_list(COMMA, constructor_params_expr); RPAREN {}


constructor_params_expr:
| value {}
| LID {}
| constructor_expr {}

/* Matching expression */
match_expr:
| BAR; match_constructor; ARROW; expr {}


match_constructor:
| LID {}
| UID; {}
| UNDERSCORE 
| value {}
| SOME; match_constructor {}
| option(UID); LPAREN; separated_nonempty_list(COMMA, match_constructor); RPAREN {}


%inline unary_op:
| SUB {}
| NOT {}
| FST {}
| SND {}


%inline binary_op:
| ADD {}
| SUB {}
| MUL {}
| DIV {}
| MOD {}
| LT {}
| GT {}
| LEQ {}
| GEQ {}
| EQ {}
| NEQ {}
| AND {}
| OR {}


%inline value:
| NONE {}
| UNIT {}
| INT {}
| TRUE {}
| FALSE {}
