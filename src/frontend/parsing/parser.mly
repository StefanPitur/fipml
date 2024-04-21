%{
  open Ast.Ast_types
  open Parser_ast

  let mutually_recursive_group_id = ref 0
  let incr_mutually_recursive_group_id () =
    mutually_recursive_group_id := !mutually_recursive_group_id + 1
  let get_mutually_recursive_group_id () = !mutually_recursive_group_id
%}

/* Tokens Definition */
%token<int> INT
%token<string> LID
%token<string> UID
%token AT
%token LPAREN
%token RPAREN
%token LCURLY
%token RCURLY
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
%token ANDFUN
%token IN
%token TYPE
%token MATCH
%token ENDMATCH
%token WITH
%token DROP
%token FREE
%token WEAK
%token INST
%token FIP
%token FBIP
%token EOF

/* Types Tokens */
%token<string> POLY
%token TYPE_INT
%token TYPE_BOOL
%token TYPE_UNIT

/* Uniqueness Attributes */
%token UNIQUE
%token SHARED

/* Precedence and associativity */
%nonassoc LT GT LEQ GEQ EQ NEQ IN
%left OR
%left AND
%right NOT
%left ADD SUB
%left MUL DIV MOD
%nonassoc SEMICOLON

/* Starting non-terminal, endpoint for calling the parser */
%start <program> program

%type<poly> poly
%type<uniqueness> uniqueness
%type<type_expr> type_expr

/* Types for Type Definitions */
%type<type_defn> type_defn
%type<type_constructor> type_constructor
%type<type_expr list>type_constructor_arguments

/* Types for Function Definitions */
%type<function_defn> function_defn
%type<param> function_param

/* Types for Expression Definitions */
%type<expr> expr
%type<pattern_expr> match_expr
%type<matched_expr> match_constructor

%type<unary_op> unary_op
%type<binary_op> binary_op
%type<value> value
%%

program:
| type_defns=list(type_defn); function_defns=list(function_defn); main_expr_option=option(block_expr); EOF { 
    TProg($startpos, type_defns, function_defns, main_expr_option)
  }

poly:
| poly=POLY { Poly($startpos, poly) }

uniqueness:
| SHARED { Shared($startpos) }
| UNIQUE { Unique($startpos) }
| poly=poly { PolyUnique($startpos, poly) }

custom_poly_arg:
| poly=poly { CustomArgPoly poly }
| SHARED { CustomArgUnique (Shared($startpos)) }
| UNIQUE { CustomArgUnique (Unique($startpos)) }
| typ=typ_no_poly { CustomArgTyp typ }
| typ=typ; AT; uniqueness=uniqueness { CustomArgTypeExpr (TAttr($startpos, typ, uniqueness)) }

typ_no_poly:
| TYPE_UNIT { TEUnit($startpos) }
| TYPE_INT { TEInt($startpos) }
| TYPE_BOOL { TEBool($startpos) }
| custom_type=LID { TECustom($startpos, [], Type_name.of_string custom_type) }
| custom_poly_arg=custom_poly_arg; custom_type=LID { TECustom($startpos, [custom_poly_arg], Type_name.of_string custom_type) }
| LPAREN; custom_poly_args=separated_nonempty_list(COMMA, custom_poly_arg); RPAREN; custom_type=LID { TECustom($startpos, custom_poly_args, Type_name.of_string custom_type) }
| LPAREN; in_type=type_expr; ARROW; out_type=type_expr; RPAREN { TEArrow($startpos, in_type, out_type) }
| LPAREN; type_expr=type_expr; MUL; type_exprs=separated_nonempty_list(MUL, type_expr); RPAREN { TETuple($startpos, type_expr :: type_exprs) }

typ:
| poly=poly { TEPoly ($startpos, poly) }
| typ_no_poly=typ_no_poly { typ_no_poly }

type_expr:
| poly=poly { TPoly(poly) }
| typ=typ; AT; uniqueness=uniqueness { TAttr($startpos, typ, uniqueness) }

/* Type Definition Production Rules */
// Type Definition Structure Production Rules 
type_defn:
| TYPE; type_name=LID; ASSIGN; type_constructors=nonempty_list(type_constructor) { 
    TType($startpos, [], Type_name.of_string type_name, type_constructors) 
  }
| TYPE; poly=poly; type_name=LID; ASSIGN; type_constructors=nonempty_list(type_constructor) { 
    TType($startpos, [poly], Type_name.of_string type_name, type_constructors) 
  }
| TYPE; LPAREN; polys=separated_nonempty_list(COMMA, poly); RPAREN; type_name=LID; ASSIGN; type_constructors=nonempty_list(type_constructor) { 
    TType($startpos, polys, Type_name.of_string type_name, type_constructors) 
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
| mut_rec=option(ANDFUN); FIP; FUN; fun_name=LID; fun_params=nonempty_list(function_param); COLON; return_type=type_expr; ASSIGN; fun_body=block_expr {
    (match mut_rec with
    | None ->  incr_mutually_recursive_group_id ()
    | _ -> ());
    TFun($startpos, get_mutually_recursive_group_id (), Some (Fip 0), Function_name.of_string fun_name, fun_params, fun_body, return_type)
  }
| mut_rec=option(ANDFUN); FIP; LPAREN; n=INT; RPAREN; FUN; fun_name=LID; fun_params=nonempty_list(function_param); COLON; return_type=type_expr; ASSIGN; fun_body=block_expr {
    (match mut_rec with
    | None ->  incr_mutually_recursive_group_id ()
    | _ -> ());
    TFun($startpos, get_mutually_recursive_group_id (), Some (Fip n), Function_name.of_string fun_name, fun_params, fun_body, return_type)
  }
| mut_rec=option(ANDFUN); FBIP; FUN; fun_name=LID; fun_params=nonempty_list(function_param); COLON; return_type=type_expr; ASSIGN; fun_body=block_expr {
    (match mut_rec with
    | None ->  incr_mutually_recursive_group_id ()
    | _ -> ());
    TFun($startpos, get_mutually_recursive_group_id(), Some (Fbip 0), Function_name.of_string fun_name, fun_params, fun_body, return_type)
  }
| mut_rec=option(ANDFUN); FBIP; LPAREN; n=INT; RPAREN; FUN; fun_name=LID; fun_params=nonempty_list(function_param); COLON; return_type=type_expr; ASSIGN; fun_body=block_expr {
    (match mut_rec with
    | None ->  incr_mutually_recursive_group_id ()
    | _ -> ());
    TFun($startpos, get_mutually_recursive_group_id(), Some (Fbip n), Function_name.of_string fun_name, fun_params, fun_body, return_type)
  }
| mut_rec=option(ANDFUN); FUN; fun_name=LID; fun_params=nonempty_list(function_param); COLON; return_type=type_expr; ASSIGN; fun_body=block_expr {
    (match mut_rec with
    | None ->  incr_mutually_recursive_group_id ()
    | _ -> ());
    TFun($startpos, get_mutually_recursive_group_id(), None, Function_name.of_string fun_name, fun_params, fun_body, return_type)
  }

function_param:
| borrowed=option(BORROWED); LPAREN; param_name=LID; COLON; param_type=type_expr; RPAREN {
    match borrowed with
    | None -> TParam(param_type, Var_name.of_string param_name, None)
    | Some _ -> TParam(param_type, Var_name.of_string param_name, Some Borrowed)
  }

/* Block Expression Definition Production Rules */
block_expr:
| LCURLY; expr=expr; RCURLY { expr }

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
| DROP; dropped_var_name=LID; SEMICOLON; expr=expr { Drop($startpos, Var_name.of_string dropped_var_name, expr) }
| FREE; k=INT; SEMICOLON; expr=expr { Free($startpos, k, expr) }
| WEAK; k=INT; SEMICOLON; expr=expr { Weak($startpos, k, expr) }
| INST; k=INT; SEMICOLON; expr=expr { Inst($startpos, k, expr) }

/* Function call / application */
| fun_name=LID; LPAREN; fun_args=separated_nonempty_list(COMMA, value); RPAREN {
    FunCall($startpos, Function_name.of_string fun_name, fun_args)
  }
| BORROWED; fun_name=LID; LPAREN; fun_owned_args=separated_nonempty_list(COMMA, value); RPAREN {
    FunApp($startpos, Var_name.of_string fun_name, fun_owned_args)
  }

/* Matching expression */
match_expr:
| BAR; pattern_expr=match_constructor; ARROW; matched_expr=block_expr { 
    MPattern($startpos, pattern_expr, matched_expr) 
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
