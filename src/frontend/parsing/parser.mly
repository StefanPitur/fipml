%{
  open Ast.Ast_types
  open Parser_ast
  (* TODO: Could add multiple types, such as CHAR, STRING, LIST... *)
%}

/* Tokens Definition */
%token<int> INT
%token<string> LID
%token<string> UID
%token LPAREN
%token RPAREN
// These have been added to handle Function Application
%token LPARENSQ
%token RPARENSQ

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
%token TYPE_BOOL
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
%start <program> program

/* Types for Type Definitions */
%type<type_expr> type_expr
%type<type_defn> type_defn
%type<type_constructor> type_constructor
%type<type_expr list>type_constructor_arguments
%%

program:
| type_defns=list(type_defn); list(function_defn); option(expr); EOF { print_string "Finished Parsing!\n"; TProg(type_defns) }


type_expr:
| TYPE_UNIT { TEUnit }
| TYPE_INT { TEInt }
| TYPE_BOOL { TEBool }
| type_expr=type_expr; TYPE_OPTION { TEOption(type_expr) }
| custom_type=LID { TECustom(custom_type) }


/* Type Definition Production Rules */
// Type Definition Structure Production Rules 
type_defn:
| TYPE; type_name=LID; ASSIGN; type_constructors=nonempty_list(type_constructor) { 
    TType($startpos, Type_name.of_string type_name, type_constructors) 
  }


// Type Definition Constructors Production Rules
type_constructor:
| BAR; type_constructor_name=UID { 
    TTypeConstructor($startpos, Constructor_name.of_string type_constructor_name, []) 
  }
| BAR; type_constructor_name=UID; type_constructor_arguments=type_constructor_arguments { 
    TTypeConstructor($startpos, Constructor_name.of_string type_constructor_name, type_constructor_arguments)
  }


// Type Definition Constructors' Arguments Production Rules
type_constructor_arguments:
| OF; type_constructor_arguments=separated_nonempty_list(MUL, type_expr) { type_constructor_arguments }
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

/* Function application */
| LID; LPARENSQ; separated_nonempty_list(COMMA, expr); RPARENSQ {}

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
