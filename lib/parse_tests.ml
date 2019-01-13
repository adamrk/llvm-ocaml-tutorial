open Core

let print_parsed s =
  printf
    !"%{sexp: [`Expr of Ast.Expr.No_binop.func | `Extern of Ast.proto | `Def \
      of Ast.Expr.No_binop.func | `Eof ]}"
    (Menhir_parser.toplevel Ocamllexer.read (Lexing.from_string s))

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
    [%expect {| |}]
