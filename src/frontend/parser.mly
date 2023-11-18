/* Header for auxiliary code */
%{
  let get_position = Parsing.symbol_start_pos
%}

/* Tokens and Types */
%token<int> INT
%token<string> ID
%token LPAREN RPAREN COMMA SEMICOLON ADD SUB MUL DIV NOT BORROWED ASSIGN LT GT
%token BAR LEQ GEQ EQUAL AND OR ARROW UNIT FST SND IF THEN ELSE LET FUN IN
%token BEGIN END TYPE MATCH DMATCH WITH EOF OF

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
