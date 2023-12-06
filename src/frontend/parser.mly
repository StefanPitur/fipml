/* Header for auxiliary code */
%{
  (* let get_position = Parsing.symbol_start_pos *)
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
%token LET
%token REC
%token FUN
%token IN
%token BEGIN
%token END
%token TYPE
%token MATCH
%token DMATCH
%token WITH
%token EOF

/* Types Tokens */
%token TYPE_INT
%token TYPE_FLOAT
%token TYPE_CHAR
%token TYPE_BOOL
%token TYPE_STRING

/* Precedence and associativity */

%left ADD SUB
%left MUL DIV

%start <'a> program
%%

program:
| list(type_defn); list(function_defn); option(main_expr); EOF { print_string "\n! Finished Parsing\n" }

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
| TYPE; ID; ASSIGN; list(type_constructor) { print_string "type_defn\n\n" }

// Type Definition Constructors Production Rules
type_constructor:
| BAR; ID; option(type_constructor_arguments) { print_string "type_constructor\n" }

// Type Definition Constructors' Arguments Production Rules
type_constructor_arguments:
| OF; separated_list(MUL, type_expr) { print_string "constructor arguments\n" }
(* --------------------------------------------------------- *)


/* Function Definition Production Rules */
function_defn:
| LET; option(REC); ID; list(function_param); ASSIGN; block_expr { print_string "function_defn\n\n" }

function_param:
| ID {}
| LPAREN; ID; COLON; type_expr; RPAREN {}

/* Main/Block Expression Definition Production Rules */
main_expr:
| UNIT { print_string "main_expr\n" }

block_expr:
| separated_list(SEMICOLON, expr) {}
| BEGIN; separated_list(SEMICOLON, expr); END { print_string "block_expression\n" }

expr:
| UNIT {}

// prog:
// | EOF { None }
// | expression

// expression:
// | INT { $1 }
// | expression PLUS expression { $1 + $3 }
// | expression MINUS expression { $1 - $3 }
// | expression TIMES expression { $1 * $3 }
// | expression DIVIDE expression { $1 / $3 }
