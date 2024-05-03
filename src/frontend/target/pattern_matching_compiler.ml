open Ast.Ast_types
open Core
open Pre_lambda
open Typing
open Result

exception EmptyPatternExpected
exception VariableExpected

type equation = matched_expr list * expr

let mock_loc : Lexing.position =
  { pos_fname = "mock"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

(* let mock_type_expr : type_expr = TPoly (Poly (mock_loc, "'_u")) *)

let fresh_var =
  let index = ref 0 in
  fun () ->
    index := !index + 1;
    Var_name.of_string (Fmt.str "_u%i" !index)

let rec partition f lst =
  match lst with
  | [] -> []
  | [ x ] -> [ [ x ] ]
  | x :: x' :: xs ->
      if Bool.( = ) (f x) (f x') then tack x (partition f (x' :: xs))
      else [ x ] :: partition f (x' :: xs)

and tack x xss = (x :: List.hd_exn xss) :: List.tl_exn xss

let arity (loc : loc) (constructor_name : Constructor_name.t)
    (constructors_env : Type_defns_env.constructors_env) : int Or_error.t =
  Type_defns_env.get_constructor_by_name loc constructor_name constructors_env
  >>= fun (Type_defns_env.ConstructorEnvEntry
            (_, _, constructor_arg_type_exprs)) ->
  Ok (List.length constructor_arg_type_exprs)

let constructors (loc : loc) (constructor_name : Constructor_name.t)
    (constructors_env : Type_defns_env.constructors_env) :
    Constructor_name.t list Or_error.t =
  Type_defns_env.get_constructor_by_name loc constructor_name constructors_env
  >>= fun (Type_defns_env.ConstructorEnvEntry (constructor_type, _, _)) ->
  let filtered_constructors_env =
    List.filter constructors_env
      ~f:(fun (ConstructorEnvEntry (constructor_env_type, _, _)) ->
        Type_name.( = ) constructor_env_type constructor_type)
  in
  Ok
    (List.map filtered_constructors_env
       ~f:(fun (ConstructorEnvEntry (_, constructor_name, _)) ->
         constructor_name))

let is_var ((patterns, _) : equation) : bool =
  match patterns with
  | [] -> raise (Invalid_argument "patterns should not be empty")
  | pattern :: _ -> ( match pattern with MVariable _ -> true | _ -> false)

let is_con (equation : equation) : bool = not (is_var equation)

let get_con ((patterns, _) : equation) : Constructor_name.t =
  match List.hd_exn patterns with
  | MConstructor (constructor_name, _) -> constructor_name
  | _ -> raise (Invalid_argument "constructor expected")

let choose c qs =
  List.filter qs ~f:(fun q -> Constructor_name.( = ) (get_con q) c)

let fatbar x _ = x

let rec match_var_con match_kind constructors_env us qs def : expr =
  match is_var (List.hd_exn qs) with
  | true -> match_var match_kind constructors_env us qs def
  | false when is_con (List.hd_exn qs) ->
      match_con match_kind constructors_env us qs def
  | _ -> raise (Invalid_argument "not allowed")

and match_var match_kind constructors_env us qs def =
  Fmt.pf Fmt.stdout "FINE ME - VAR RULE!\n";
  let u = List.hd_exn us in
  let us = List.tl_exn us in
  Fmt.pf Fmt.stdout "Print u : %s@." (Var_name.to_string u);
  Fmt.pf Fmt.stdout "Print us:@.";
  List.iter us ~f:(fun u ->
      Fmt.pf Fmt.stdout "    u : %s@." (Var_name.to_string u));
  Fmt.pf Fmt.stdout "\n\n";
  let updated_qs =
    List.map qs ~f:(fun (patterns, expr) ->
        Or_error.ok_exn
          (match patterns with
          | MVariable v :: patterns -> Ok (patterns, var_subst v u expr)
          | _ -> Or_error.of_exn VariableExpected))
  in
  Fmt.pf Fmt.stdout "Updates qs :@.";
  pprint_equations updated_qs;
  Fmt.pf Fmt.stdout "Finish updates qs\n@.";
  compile_pattern_matching match_kind constructors_env us updated_qs def

and match_con match_kind constructors_env us qs def =
  Fmt.pf Fmt.stdout "FINE ME - CONSTR RULE!\n";
  let constructor_name = get_con (List.hd_exn qs) in
  let cs =
    Or_error.ok_exn (constructors mock_loc constructor_name constructors_env)
  in
  Fmt.pf Fmt.stdout "Constructor name - %s@."
    (Constructor_name.to_string constructor_name);
  Fmt.pf Fmt.stdout "Constructors:@.";
  List.iter cs ~f:(fun c ->
      Fmt.pf Fmt.stdout "    Constructor name - %s@."
        (Constructor_name.to_string c));
  let u = List.hd_exn us in
  let us = List.tl_exn us in
  Fmt.pf Fmt.stdout "Print u : %s@." (Var_name.to_string u);
  Fmt.pf Fmt.stdout "Print us:@.";
  List.iter us ~f:(fun u ->
      Fmt.pf Fmt.stdout "    u : %s@." (Var_name.to_string u));
  Fmt.pf Fmt.stdout "\n\n";
  let patterns =
    List.map cs ~f:(fun c ->
        match_clause match_kind constructors_env c (u :: us) (choose c qs) def)
  in
  Match (match_kind, u, patterns)

and match_clause match_kind constructors_env c us qs def =
  Fmt.pf Fmt.stdout "FINE ME - CLAUSE RULE!\n";
  let k = Or_error.ok_exn (arity mock_loc c constructors_env) in
  Fmt.pf Fmt.stdout "arity - %d@." k;
  let us = List.tl_exn us in
  let us' = List.init k ~f:(fun _ -> fresh_var ()) in
  let qs' =
    List.map qs ~f:(fun (ps, e) ->
        match ps with
        | MConstructor (mc, ps') :: ps when Constructor_name.( = ) mc c ->
            (ps' @ ps, e)
        | _ -> raise (Invalid_argument "incorrect patterns for match clause"))
  in
  MPattern
    ( MConstructor (c, List.map us' ~f:(fun u' -> MVariable u')),
      compile_pattern_matching match_kind constructors_env (us' @ us) qs' def )

and compile_pattern_matching (match_kind : match_kind)
    (constructors_env : Type_defns_env.constructors_env) (us : Var_name.t list)
    (qs : equation list) (def : expr) : expr =
  let expr =
    match us with
    | [] ->
        let exprs =
          List.map qs ~f:(fun (patterns, expr) ->
              Or_error.ok_exn
                (match patterns with
                | [] -> Ok expr
                | _ -> Or_error.of_exn EmptyPatternExpected))
        in
        List.fold_right exprs ~init:def ~f:fatbar
    | u :: us ->
        let partitioned_qs = partition is_var qs in
        List.iter partitioned_qs ~f:pprint_equations;
        Fmt.pf Fmt.stdout "DONE\n<><><><><><><><><><><><><><><><><>\n\n";
        List.fold_right partitioned_qs ~init:def
          ~f:(match_var_con match_kind constructors_env (u :: us))
  in
  Fmt.pf Fmt.stdout "exit compile pattern matching:@.";
  Pprint_pre_lambda.pprint_pre_lambda_expr Fmt.stdout ~indent:"    " expr;
  Fmt.pf Fmt.stdout "exit compile pattern matching!@.";
  expr

and pprint_equation ((patterns, expr) : equation) : unit =
  Fmt.pf Fmt.stdout "Equation entry - \n";
  List.iter patterns
    ~f:
      (Pprint_pre_lambda.pprint_pre_lambda_matched_expr Fmt.stdout
         ~indent:"    ");
  Fmt.pf Fmt.stdout "\n";
  Pprint_pre_lambda.pprint_pre_lambda_expr Fmt.stdout ~indent:"" expr;
  Fmt.pf Fmt.stdout "\n\n"

and pprint_equations (equations : equation list) : unit =
  Fmt.pf Fmt.stdout "Equation List:\n";
  List.iter equations ~f:pprint_equation;
  Fmt.pf Fmt.stdout "============================================\n\n"
