open Core

let supplier =
  Menhir_parser.MenhirInterpreter.lexer_lexbuf_to_supplier Ocamllexer.read
    (Lexing.from_channel In_channel.stdin)

let new_incremental () = Menhir_parser.Incremental.single_expr Lexing.dummy_pos

let run_main () =
  let rec run_loop () =
    let incremental = new_incremental () in
    let ast = Menhir_parser.MenhirInterpreter.loop supplier incremental in
    printf !"%{sexp: Ast.Expr.t}\n" (Ast.Expr.of_no_binop ast) ;
    Out_channel.flush (Out_channel.stdout);
    run_loop ()
  in
  Hashtbl.add_exn Ast.binop_precedence ~key:'<' ~data:10 ;
  Hashtbl.add_exn Ast.binop_precedence ~key:'+' ~data:20 ;
  Hashtbl.add_exn Ast.binop_precedence ~key:'-' ~data:20 ;
  Hashtbl.add_exn Ast.binop_precedence ~key:'*' ~data:40 ;
  run_loop ()
