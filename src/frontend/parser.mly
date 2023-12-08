%{
  (* Header for auxiliary code *)
%}

/* Tokens Definition */
%token<int> INT
%token<string> ID
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
%token EOF

/* Types Tokens */
%token TYPE_INT
%token TYPE_FLOAT
%token TYPE_CHAR
%token TYPE_BOOL
%token TYPE_STRING

/* Precedence and associativity */
%right FST SND
%left ADD SUB
%left MUL DIV

/* Starting non-terminal, endpoint for calling the parser */
%start <unit> program
%%

program:
| list(type_defn); list(function_defn); option(expr); EOF { print_string "\n! Finished Parsing\n" }

// Need to expand this to accepting types such as - int ref list option
type_expr:
| TYPE_INT {}
| TYPE_FLOAT {}
| TYPE_CHAR {}
| TYPE_BOOL {}
| TYPE_STRING {}
| ID {}


/* Type Definition Production Rules */
// Type Definition Structure Production Rules 
type_defn:
| TYPE; ID; ASSIGN; nonempty_list(type_constructor) {}

// Type Definition Constructors Production Rules
type_constructor:
| BAR; ID; option(type_constructor_arguments) {}

// Type Definition Constructors' Arguments Production Rules
type_constructor_arguments:
| OF; separated_nonempty_list(MUL, type_expr) {}
(* --------------------------------------------------------- *)


/* Function Definition Production Rules */
function_defn:
| FUN; option(REC); ID; list(function_param); ASSIGN; block_expr { print_string "function_defn\n\n" }

function_param:
| option(BORROWED); ID {}
| LPAREN; option(BORROWED); ID; COLON; type_expr; RPAREN {}

/* Block Expression Definition Production Rules */
block_expr:
| separated_list(SEMICOLON, expr) {} // might want to remove this, as it is not really a block
| BEGIN; separated_list(SEMICOLON, expr); END { print_string "block_expression\n" }

expr:
| UNIT {}
| INT {}
| TRUE {}
| FALSE {}
| ID {}
| unary_op; expr {}
| expr; binary_op; expr {}
| LPAREN; expr; RPAREN {}
| LPAREN; expr; COMMA expr; RPAREN {}
| LET; ID; ASSIGN; expr; IN {}

/* Control Flow - IF statements */
| IF; expr; THEN; expr; ENDIF {}
| IF; expr; THEN; expr; ELSE expr; ENDIF {}

/* Control Flow - MATCH / DMATCH statements */
| MATCH; ID; WITH; match_expr+; ENDMATCH {}
| DMATCH; ID; WITH; match_expr+; ENDMATCH {}

match_expr:
| BAR; match_constructor; ARROW; expr {}

match_constructor:
| ID {}
| UNDERSCORE 
| value {}
| option(ID); LPAREN; separated_list(COMMA, match_constructor); RPAREN {}

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
| INT {}
| TRUE {}
| FALSE {}
