open Core

let%expect_test "expression: value" =
  let values =
    [
      "SimpleConstructor";
      "ComplexConstructor(1, SimpleConstructor)";
      "()";
      "1";
      "true";
      "false";
      "x";
    ]
  in
  let source_codes = List.map values ~f:(fun value -> "{ " ^ value ^ " }") in
  List.iter source_codes ~f:Pprint_parser_ast.pprint_parser_ast;
  [%expect
    {|
    Program
        Main
            Expr: UnboxedSingleton
                Value: Constructor: SimpleConstructor
                    ()
    Program
        Main
            Expr: UnboxedSingleton
                Value: Constructor: ComplexConstructor
                    ConstructorArg
                        Value: Int: 1
                    ConstructorArg
                        Value: Constructor: SimpleConstructor
                            ()
    Program
        Main
            Expr: UnboxedSingleton
                Value: Unit
    Program
        Main
            Expr: UnboxedSingleton
                Value: Int: 1
    Program
        Main
            Expr: UnboxedSingleton
                Value: Bool: true
    Program
        Main
            Expr: UnboxedSingleton
                Value: Bool: false
    Program
        Main
            Expr: UnboxedSingleton
                Value: Var: x |}]
