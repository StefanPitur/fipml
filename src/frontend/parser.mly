/* Header for auxiliary code */
%{
  let get_position = Parsing.symbol_start_pos
%}

/* Tokens Definition */
%token<int> INT
%token<string> ID
%token LPAREN
%token RPAREN
%token COMMA
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
%token FUN
%token IN
%token BEGIN
%token END
%token TYPE
%token MATCH
%token DMATCH
%token WITH
%token EOF

/* Precedence and associativity */

%left ADD SUB
%left MUL DIV

// %start prog
%%

// prog:
// | EOF { None }
// | expression

// expression:
// | INT { $1 }
// | expression PLUS expression { $1 + $3 }
// | expression MINUS expression { $1 - $3 }
// | expression TIMES expression { $1 * $3 }
// | expression DIVIDE expression { $1 / $3 }
