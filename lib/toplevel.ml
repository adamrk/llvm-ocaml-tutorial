open Core

let run_main in_channel ~the_fpm ~the_execution_engine =
  let anonymous_func_count = ref 0 in
  let supplier =
    Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.read
      (Lexing.from_channel in_channel)
  in
  let rec run_loop the_fpm the_execution_engine supplier =
    let incremental = Parser.Incremental.toplevel Lexing.dummy_pos in
    printf "\nready> " ;
    Out_channel.flush stdout ;
    ( try
        match Parser.MenhirInterpreter.loop supplier incremental with
        | `Expr ast ->
            (* Evaluate a top-level expression into an anonymous function. *)
            let func = Ast.func_of_no_binop_func ast in
            printf "parsed a toplevel expression" ;
            Out_channel.flush stdout ;
            Llvm_executionengine.add_module Codegen.the_module
              the_execution_engine ;
            anonymous_func_count := !anonymous_func_count + 1 ;
            let tmp_name = sprintf "__toplevel%d" !anonymous_func_count in
            let tmp_func = Ast.set_func_name tmp_name func in
            let the_function = Codegen.codegen_func the_fpm tmp_func in
            Llvm.dump_value the_function ;
            (* JIT the function, returning a function pointer. *)
            let fp =
              Llvm_executionengine.get_function_address tmp_name
                (Foreign.funptr Ctypes.(void @-> returning double))
                the_execution_engine
            in
            printf "Evaluated to %f" (fp ()) ;
            Llvm_executionengine.remove_module Codegen.the_module
              the_execution_engine
        | `Extern ext ->
            printf "parsed an extern" ;
            (* printf !"%{sexp: Ast.proto}\n" ext; *)
            Out_channel.flush stdout ;
            Llvm.dump_value (Codegen.codegen_proto ext)
        | `Def def ->
            printf "parsed a definition" ;
            let func = Ast.func_of_no_binop_func def in
            (* printf !"%{sexp: Ast.func}\n" func; *)
            Out_channel.flush stdout ;
            Llvm.dump_value (Codegen.codegen_func the_fpm func)
        | `Eof ->
            printf "\n\n" ;
            printf "reached eof\n" ;
            printf "module dump:\n" ;
            Out_channel.flush Out_channel.stdout ;
            (* Print out all the generated code. *)
            Llvm.dump_module Codegen.the_module ;
            exit 0
      with e ->
        (* Skip expression for error recovery. *)
        printf !"\nencountered an error %{sexp: exn}" e ) ;
    Out_channel.flush Out_channel.stdout ;
    run_loop the_fpm the_execution_engine supplier
  in
  run_loop the_fpm the_execution_engine supplier

let main input =
  (* Install standard binary operators.
   * 1 is the lowest precedence. *)
  Hashtbl.add_exn Ast.binop_precedence ~key:'=' ~data:2 ;
  Hashtbl.add_exn Ast.binop_precedence ~key:'<' ~data:10 ;
  Hashtbl.add_exn Ast.binop_precedence ~key:'+' ~data:20 ;
  Hashtbl.add_exn Ast.binop_precedence ~key:'-' ~data:20 ;
  Hashtbl.add_exn Ast.binop_precedence ~key:'*' ~data:40 ;
  (* Create the JIT *)
  let the_execution_engine =
    ( match Llvm_executionengine.initialize () with
    | true -> ()
    | false -> raise_s [%message "failed to initialize"] ) ;
    Llvm_executionengine.create Codegen.the_module
  in
  let the_fpm = Llvm.PassManager.create_function Codegen.the_module in
  (* Promote allocas to registers. *)
  Llvm_scalar_opts.add_memory_to_register_promotion the_fpm ;
  (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
  Llvm_scalar_opts.add_instruction_combination the_fpm ;
  (* reassociate expressions. *)
  Llvm_scalar_opts.add_reassociation the_fpm ;
  (* Eliminate Common SubExpressions. *)
  Llvm_scalar_opts.add_gvn the_fpm ;
  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  Llvm_scalar_opts.add_cfg_simplification the_fpm ;
  Llvm.PassManager.initialize the_fpm |> ignore ;
  match input with
  | `Stdin -> run_main ~the_execution_engine ~the_fpm In_channel.stdin
  | `File file ->
      In_channel.with_file file ~f:(run_main ~the_execution_engine ~the_fpm)
