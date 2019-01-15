open Core

let print_parsed s =
  printf
    !"%{sexp: [`Expr of Ast.Expr.No_binop.func | `Extern of Ast.proto | `Def \
      of Ast.Expr.No_binop.func | `Eof ]}"
    (Parser.toplevel Lexer.read (Lexing.from_string s))

let%expect_test _ =
  print_parsed "LHS < RHS;" ;
  [%expect
    {|
    (Expr
     (Function (Prototype "" ())
      (Bin_list (Variable LHS) ((< -1 (Variable RHS)))))) |}] ;
  print_parsed "LHS < RHS | LHS > RHS;" ;
  [%expect
    {|
    (Expr
     (Function (Prototype "" ())
      (Bin_list (Variable LHS)
       ((< -1 (Variable RHS)) (| -1 (Variable LHS)) (> -1 (Variable RHS)))))) |}];
  print_parsed "def binary = 9 (LHS RHS) !(LHS < RHS | LHS > RHS);";
  [%expect {|
    (Def
     (Function (BinOpPrototype binary= (LHS RHS) 9)
      (Unary !
       (Bin_list (Variable LHS)
        ((< -1 (Variable RHS)) (| -1 (Variable LHS)) (> -1 (Variable RHS))))))) |}];
  print_parsed "";
  [%expect {| Eof |}];
  print_parsed "!a | x;";
  [%expect
    {|
    (Expr
     (Function (Prototype "" ())
      (Bin_list (Unary ! (Variable a)) ((| -1 (Variable x)))))) |}] ;
  print_parsed "if x > 0 then 1 else x + 10;";
  [%expect {|
    (Expr
     (Function (Prototype "" ())
      (If (Bin_list (Variable x) ((> -1 (Number 0)))) (Number 1)
       (Bin_list (Variable x) ((+ -1 (Number 10))))))) |}];
  print_parsed "5 + #some comment\n5;";
  [%expect {| (Expr (Function (Prototype "" ()) (Bin_list (Number 5) ((+ -1 (Number 5)))))) |}];
