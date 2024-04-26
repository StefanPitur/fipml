let%expect_test "mutually recursive function defns" =
  let source_code =
    "\n\
    \    fun is_even (x : int @ shared) : bool @ shared = {\n\
    \      let pred_x = x - 1 in\n\
    \      if x > 0 then { \n\
    \          is_odd (pred_x) \n\
    \        } else {\n\
    \          true \n\
    \        } \n\
    \      endif\n\
    \    }\n\n\
    \    and fun is_odd (x : int @ shared) : bool @ shared = {\n\
    \      let pred_x = x - 1 in\n\
    \      if x > 0 then { \n\
    \          is_even (pred_x) \n\
    \        } else {\n\
    \          false\n\
    \        } \n\
    \      endif\n\
    \    }\n\
    \    fun add (x : int @ shared) (y : int @ shared) : int @ shared = { x + \
     y }\n\
    \      "
  in
  Pprint_parser_ast.pprint_parser_ast source_code;
  [%expect
    {|
      Program
          Function Name: is_even
          Mutually Recursive Group Id: 1
          Param Types:
              Type Expr: Int @ shared
              OwnedParam: x
          Return Type: Bool @ shared
          Function Body Expr
              Expr: Let vars: (pred_x) =
                  Binary Op: -
                      LeftExpr
                      Expr: UnboxedSingleton
                          Value: Var: x
                      RightExpr
                      Expr: UnboxedSingleton
                          Value: Int: 1
                  Expr: IfElse
                      Binary Op: >
                          LeftExpr
                          Expr: UnboxedSingleton
                              Value: Var: x
                          RightExpr
                          Expr: UnboxedSingleton
                              Value: Int: 0
                      Then
                      Expr: FunCall
                          Function Name: is_odd
                          FunCall Args:
                              Value: Var: pred_x
                      Else
                      Expr: UnboxedSingleton
                          Value: Bool: true
          Function Name: is_odd
          Mutually Recursive Group Id: 1
          Param Types:
              Type Expr: Int @ shared
              OwnedParam: x
          Return Type: Bool @ shared
          Function Body Expr
              Expr: Let vars: (pred_x) =
                  Binary Op: -
                      LeftExpr
                      Expr: UnboxedSingleton
                          Value: Var: x
                      RightExpr
                      Expr: UnboxedSingleton
                          Value: Int: 1
                  Expr: IfElse
                      Binary Op: >
                          LeftExpr
                          Expr: UnboxedSingleton
                              Value: Var: x
                          RightExpr
                          Expr: UnboxedSingleton
                              Value: Int: 0
                      Then
                      Expr: FunCall
                          Function Name: is_even
                          FunCall Args:
                              Value: Var: pred_x
                      Else
                      Expr: UnboxedSingleton
                          Value: Bool: false
          Function Name: add
          Mutually Recursive Group Id: 2
          Param Types:
              Type Expr: Int @ shared
              OwnedParam: x
              Type Expr: Int @ shared
              OwnedParam: y
          Return Type: Int @ shared
          Function Body Expr
              Binary Op: +
                  LeftExpr
                  Expr: UnboxedSingleton
                      Value: Var: x
                  RightExpr
                  Expr: UnboxedSingleton
                      Value: Var: y |}]
