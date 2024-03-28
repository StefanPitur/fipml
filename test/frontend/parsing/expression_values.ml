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
  let source_codes =
    List.map values ~f:(fun value -> "begin " ^ value ^ " end")
  in
  List.iter source_codes ~f:Pprint_parser_ast.pprint_parser_ast;
  [%expect
    {|
    Program
        Main Block
            Expr: UnboxedSingleton
                Value: Constructor: SimpleConstructor
                    ()
    Program
        Main Block
            Expr: UnboxedSingleton
                Value: Constructor: ComplexConstructor
                    ConstructorArg
                        Value: Int: 1
                    ConstructorArg
                        Value: Constructor: SimpleConstructor
                            ()
    Program
        Main Block
            Expr: UnboxedSingleton
                Value: Unit
    Program
        Main Block
            Expr: UnboxedSingleton
                Value: Int: 1
    Program
        Main Block
            Expr: UnboxedSingleton
                Value: Bool: true
    Program
        Main Block
            Expr: UnboxedSingleton
                Value: Bool: false
    Program
        Main Block
            Expr: UnboxedSingleton
                Value: Var: x |}]
