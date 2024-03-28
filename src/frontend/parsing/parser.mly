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
%token IF
%token THEN
%token ELSE
%token ENDIF
%token TRUE
%token FALSE
%token LET
%token FUN
%token IN
%token BEGIN
%token END
%token TYPE
%token MATCH
%token ENDMATCH
%token WITH
%token DROP
%token FREE
%token FIP
%token FBIP
%token EOF

/* Types Tokens */
%token TYPE_INT
%token TYPE_BOOL
%token TYPE_UNIT

/* Precedence and associativity */
%nonassoc LT GT LEQ GEQ EQ NEQ IN
%left OR
%left AND
%right NOT
%left ADD SUB
%left MUL DIV MOD
%right ARROW

/* Starting non-terminal, endpoint for calling the parser */
%start <program> program

%type<type_expr> type_expr

/* Types for Type Definitions */
%type<type_defn> type_defn
%type<type_constructor> type_constructor
%type<type_expr list>type_constructor_arguments

/* Types for Function Definitions */
%type<function_defn> function_defn
%type<(param list * param list)> function_params

/* Types for Expression Definitions */
%type<block_expr> block_expr

%type<expr> expr
%type<pattern_expr> match_expr
%type<matched_expr> match_constructor

%type<unary_op> unary_op
%type<binary_op> binary_op
%type<value> value
%%

program:
| type_defns=list(type_defn); function_defns=list(function_defn); block_expr=option(block_expr); EOF { 
    TProg($startpos, type_defns, function_defns, block_expr)
  }


type_expr:
| TYPE_UNIT { TEUnit($startpos) }
| TYPE_INT { TEInt($startpos) }
| TYPE_BOOL { TEBool($startpos) }
| custom_type=LID { TECustom($startpos, Type_name.of_string custom_type) }
| in_type=type_expr; ARROW; out_type=type_expr { TEArrow($startpos, in_type, out_type) }
| LPAREN; in_type=type_expr; ARROW; out_type=type_expr; RPAREN { TEArrow($startpos, in_type, out_type) }


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
| FIP; FUN; fun_name=LID; fun_params=function_params; COLON; return_type=type_expr; ASSIGN; fun_body=block_expr {
    let (borrowed_params, owned_params) = fun_params in
    TFun($startpos, Some (Fip 0), Function_name.of_string fun_name, borrowed_params, owned_params, fun_body, return_type)
  }
| FIP; LPAREN; n=INT; RPAREN; FUN; fun_name=LID; fun_params=function_params; COLON; return_type=type_expr; ASSIGN; fun_body=block_expr {
    let (borrowed_params, owned_params) = fun_params in
    TFun($startpos, Some (Fip n), Function_name.of_string fun_name, borrowed_params, owned_params, fun_body, return_type)
  }
| FBIP; FUN; fun_name=LID; fun_params=function_params; COLON; return_type=type_expr; ASSIGN; fun_body=block_expr {
    let (borrowed_params, owned_params) = fun_params in
    TFun($startpos, Some (Fbip 0), Function_name.of_string fun_name, borrowed_params, owned_params, fun_body, return_type)
  }
| FBIP; LPAREN; n=INT; RPAREN; FUN; fun_name=LID; fun_params=function_params; COLON; return_type=type_expr; ASSIGN; fun_body=block_expr {
    let (borrowed_params, owned_params) = fun_params in
    TFun($startpos, Some (Fbip n), Function_name.of_string fun_name, borrowed_params, owned_params, fun_body, return_type)
  }
| FUN; fun_name=LID; fun_params=function_params; COLON; return_type=type_expr; ASSIGN; fun_body=block_expr {
    let (borrowed_params, owned_params) = fun_params in
    TFun($startpos, None, Function_name.of_string fun_name, borrowed_params, owned_params, fun_body, return_type)
  }


function_params:
| borrowed_params=nonempty_list(function_borrowed_param); owned_params=list(function_owned_param) {(borrowed_params, owned_params)}
| owned_params=nonempty_list(function_owned_param) {([], owned_params)}

function_owned_param:
| LPAREN; param_name=LID; COLON; param_type=type_expr; RPAREN {
    TParam(param_type, Var_name.of_string param_name, None)
  }
  
function_borrowed_param:
| BORROWED; LPAREN; param_name=LID; COLON; param_type=type_expr; RPAREN {
    TParam(param_type, Var_name.of_string param_name, Some Borrowed)
  }


/* Block Expression Definition Production Rules */
block_expr:
| BEGIN; exprs=separated_nonempty_list(SEMICOLON, expr); END { Block($startpos, exprs) }

/* Value Definition Production Rules */
value:
| UNIT { Unit($startpos) }
| n=INT { Integer($startpos, n) }
| TRUE { Boolean($startpos, true) }
| FALSE { Boolean($startpos, false) }
| var_name=LID { Variable($startpos, Var_name.of_string var_name) }
| constructor_name=UID { Constructor($startpos, Constructor_name.of_string constructor_name, []) }
| constructor_name=UID; LPAREN; constructor_args=separated_nonempty_list(COMMA, value); RPAREN {
    Constructor($startpos, Constructor_name.of_string constructor_name, constructor_args)
  }

expr:
/* Unboxed Tuples */
| value=value { UnboxedSingleton($startpos, value) }
| LPAREN; values=separated_nonempty_list(COMMA, value); RPAREN { UnboxedTuple($startpos, values) }

/* Convoluted expressions */
| unary_op=unary_op; expr=expr { UnOp($startpos, unary_op, expr) }
| expr_left=expr; binary_op=binary_op; expr_right=expr {
    BinaryOp($startpos, binary_op, expr_left, expr_right)
  }

| LET; var_name=LID; ASSIGN; var_expr=expr; IN; var_scope=expr {
    Let($startpos, [Var_name.of_string var_name], var_expr, var_scope)
  }
| LET; LPAREN; var_names=separated_nonempty_list(COMMA, LID); RPAREN; ASSIGN; var_expr=expr; IN; var_scope=expr {
    let vars = List.map (fun var_name -> Var_name.of_string var_name) var_names in
    Let($startpos, vars, var_expr, var_scope)
  }

/* Control Flow - IF statements */
| IF; cond_expr=expr; THEN; then_expr=block_expr; ENDIF {
    If($startpos, cond_expr, then_expr)
  }
| IF; cond_expr=expr; THEN; then_expr=block_expr; ELSE else_expr=block_expr; ENDIF {
    IfElse($startpos, cond_expr, then_expr, else_expr)
  }

/* Control Flow - MATCH */
| MATCH; match_var_name=LID; WITH; pattern_exprs=nonempty_list(match_expr); ENDMATCH {
    Match($startpos, Var_name.of_string match_var_name, pattern_exprs)
  }

/* Memory deallocation operators */
| DROP; dropped_var_name=LID { Drop($startpos, Var_name.of_string dropped_var_name) }
| FREE; freed_reuse_credit_size=LID { Free($startpos, Variable($startpos, Var_name.of_string freed_reuse_credit_size)) }
| FREE; freed_reuse_credit_size=INT { Free($startpos, Integer($startpos, freed_reuse_credit_size)) }

/* Function call / application */
| fun_name=LID; LPAREN; fun_borrowed_args=option(expr); SEMICOLON; fun_owned_args=option(expr); RPAREN {
    FunCall($startpos, Function_name.of_string fun_name, fun_borrowed_args, fun_owned_args)
  }
| fun_name=LID; LPAREN; UNDERSCORE; SEMICOLON; fun_owned_args=option(expr); RPAREN {
    FunApp($startpos, Var_name.of_string fun_name, fun_owned_args)
  }

/* Matching expression */
match_expr:
| BAR; matched_expr=match_constructor; ARROW; block_expr=block_expr { 
    MPattern($startpos, matched_expr, block_expr) 
  }


match_constructor:
| UNDERSCORE { MUnderscore($startpos) }
| var_name=LID { MVariable($startpos, Var_name.of_string var_name) }
| constructor_name=UID; { 
    MConstructor($startpos, Constructor_name.of_string constructor_name, []) 
  }
| constructor_name=UID; LPAREN; constructor_args=separated_nonempty_list(COMMA, match_constructor); RPAREN {
    MConstructor($startpos, Constructor_name.of_string constructor_name, constructor_args)
  }

%inline unary_op:
| SUB { UnOpNeg }
| NOT { UnOpNot }


%inline binary_op:
| ADD { BinOpPlus }
| SUB { BinOpMinus }
| MUL { BinOpMult }
| DIV { BinOpDiv }
| MOD { BinOpMod }
| LT { BinOpLt }
| GT { BinOpGt }
| LEQ { BinOpLeq }
| GEQ { BinOpGeq }
| EQ { BinOpEq }
| NEQ { BinOpNeq }
| AND { BinOpAnd }
| OR { BinOpOr }
