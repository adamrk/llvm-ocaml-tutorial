open Core

let supplier =
  Menhir_parser.MenhirInterpreter.lexer_lexbuf_to_supplier Ocamllexer.read
    (Lexing.from_channel In_channel.stdin)

let new_incremental () = Menhir_parser.Incremental.toplevel Lexing.dummy_pos

let run_main () =
  let rec run_loop () =
    let incremental = new_incremental () in
    printf "\nready> ";
    Out_channel.flush stdout;
    ( match Menhir_parser.MenhirInterpreter.loop supplier incremental with
    | `Expr ast ->
        printf "parsed a toplevel expression";
        let func = (Ast.func_of_no_binop_func ast) in
        (* printf !"%{sexp: Ast.func}\n" func; *)
        Out_channel.flush stdout;
        Llvm.dump_value (Codegen.codegen_func func)
    | `Extern ext -> 
        printf "parsed an extern";
        (* printf !"%{sexp: Ast.proto}\n" ext; *)
        Out_channel.flush stdout;
        Llvm.dump_value (Codegen.codegen_proto ext)
    | `Def def -> 
        printf "parsed a definition";
        let func = (Ast.func_of_no_binop_func def) in
        (* printf !"%{sexp: Ast.func}\n" func; *)
        Out_channel.flush stdout;
        Llvm.dump_value (Codegen.codegen_func func) 
    ) ;
    Out_channel.flush Out_channel.stdout ;
    run_loop ()
  in
  Hashtbl.add_exn Ast.binop_precedence ~key:'<' ~data:10 ;
  Hashtbl.add_exn Ast.binop_precedence ~key:'+' ~data:20 ;
  Hashtbl.add_exn Ast.binop_precedence ~key:'-' ~data:20 ;
  Hashtbl.add_exn Ast.binop_precedence ~key:'*' ~data:40 ;
  run_loop ()
