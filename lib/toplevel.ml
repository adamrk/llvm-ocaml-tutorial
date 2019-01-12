open Core

let setup_execution_engine () =
  ( match Llvm_executionengine.initialize () with
  | true -> ()
  | false -> raise_s [%message "failed to initialize"] ) ;
  Llvm_executionengine.create Codegen.the_module

let setup_pass_manager () =
  let the_fpm = Llvm.PassManager.create_function Codegen.the_module in
  Llvm_scalar_opts.add_memory_to_register_promotion the_fpm ;
  Llvm_scalar_opts.add_instruction_combination the_fpm ;
  Llvm_scalar_opts.add_reassociation the_fpm ;
  Llvm_scalar_opts.add_gvn the_fpm ;
  Llvm_scalar_opts.add_cfg_simplification the_fpm ;
  Llvm.PassManager.initialize the_fpm |> ignore ;
  the_fpm

let new_incremental () = Menhir_parser.Incremental.toplevel Lexing.dummy_pos

let run_main () =
  let anonymous_func_count = ref 0 in
  let rec run_loop the_fpm the_execution_engine supplier =
    let incremental = new_incremental () in
    printf "\nready> " ;
    Out_channel.flush stdout ;
    ( match Menhir_parser.MenhirInterpreter.loop supplier incremental with
    | `Expr ast ->
        printf "parsed a toplevel expression" ;
        let func = Ast.func_of_no_binop_func ast in
        (* printf !"%{sexp: Ast.func}\n" func; *)
        Out_channel.flush stdout ;
        Llvm_executionengine.add_module Codegen.the_module the_execution_engine ;
        anonymous_func_count := !anonymous_func_count + 1 ;
        let tmp_name = sprintf "_func_%d" !anonymous_func_count in
        let tmp_func = Ast.set_func_name tmp_name func in
        let the_function = Codegen.codegen_func the_fpm tmp_func in
        Llvm.dump_value the_function ;
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
        Llvm.dump_value (Codegen.codegen_func the_fpm func) ) ;
    Out_channel.flush Out_channel.stdout ;
    run_loop the_fpm the_execution_engine supplier
  in
  Hashtbl.add_exn Ast.binop_precedence ~key:'<' ~data:10 ;
  Hashtbl.add_exn Ast.binop_precedence ~key:'+' ~data:20 ;
  Hashtbl.add_exn Ast.binop_precedence ~key:'-' ~data:20 ;
  Hashtbl.add_exn Ast.binop_precedence ~key:'*' ~data:40 ;
  let supplier =
    Menhir_parser.MenhirInterpreter.lexer_lexbuf_to_supplier Ocamllexer.read
      (Lexing.from_channel In_channel.stdin)
  in
  let the_execution_engine = setup_execution_engine () in
  let the_fpm = setup_pass_manager () in
  run_loop the_fpm the_execution_engine supplier
