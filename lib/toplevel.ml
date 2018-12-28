open Core
open Async

let anonymous_func_count = ref 0

let flushed () = Out_channel.flush Out_channel.stdout

let setup f =
  ( match Llvm_executionengine.initialize () with
  | true -> ()
  | false -> raise_s [%message "failed to initialize"] ) ;
  let the_execution_engine = Llvm_executionengine.create Codegen.the_module in
  let the_fpm = Llvm.PassManager.create_function Codegen.the_module in
  Llvm_scalar_opts.add_memory_to_register_promotion the_fpm ;
  Llvm_scalar_opts.add_instruction_combination the_fpm ;
  Llvm_scalar_opts.add_reassociation the_fpm ;
  Llvm_scalar_opts.add_gvn the_fpm ;
  Llvm_scalar_opts.add_cfg_simplification the_fpm ;
  Llvm.PassManager.initialize the_fpm |> ignore ;
  f the_fpm the_execution_engine

let get_all =
  let open Angstrom in
  Lexer.get_definition
  >>| (fun d -> `Def d)
  <|> (Lexer.get_extern >>| fun e -> `Ext e)
  <|> (Lexer.get_toplevel >>| fun t -> `Top t)
  <|> skip_while (fun c -> Char.( <> ) c '\n') *> end_of_line *> return `Failed

let handle_inputs the_fpm the_execution_engine input =
  match input with
  | `Def function_def ->
      printf "parsed a definition\n" ;
      let ir = Codegen.codegen_func the_fpm function_def in
      Llvm.dump_value ir ; return ()
  | `Ext extern_def ->
      printf "parsed an extern\n" ;
      let ir = Codegen.codegen_proto extern_def in
      Llvm.dump_value ir ; return ()
  | `Top expr ->
      printf "\nparsed a toplevel expr\n" ;
      Llvm_executionengine.add_module Codegen.the_module the_execution_engine ;
      flushed () ;
      anonymous_func_count := !anonymous_func_count + 1 ;
      let tmp_name = sprintf "_anonymous_func_%d" !anonymous_func_count in
      let tmp_func = Lexer.Function.set_name tmp_name expr in
      let the_function = Codegen.codegen_func the_fpm tmp_func in
      Llvm.dump_value the_function ;
      let fp =
        Llvm_executionengine.get_function_address tmp_name
          (Foreign.funptr Ctypes.(void @-> returning double))
          the_execution_engine
      in
      printf "Evaluated to %f\n" (fp ()) ;
      Llvm_executionengine.remove_module Codegen.the_module
        the_execution_engine ;
      return ()
  | `Failed ->
      printf "unable to parse this line\n" ;
      return ()

let rec main_loop the_fpm the_execution_engine =
  printf "ready> " ;
  Async_unix.Writer.flushed (Lazy.force Async_unix.Writer.stdout)
  >>= fun () ->
  Async_unix.Reader.read_line (Lazy.force Async_unix.Reader.stdin)
  >>= function
  | `Ok string ->
      Angstrom.parse_string (Angstrom.many get_all) string
      |> (function
           | Ok inputs ->
               Deferred.List.iter inputs
                 ~f:(handle_inputs the_fpm the_execution_engine)
           | Error error ->
               print_s
                 [%message
                   "got an error with input" (error : string) (string : string)] ;
               return ())
      >>= fun () -> main_loop the_fpm the_execution_engine
  | `Eof ->
      Llvm.dump_module Codegen.the_module ;
      raise_s [%message "reached eof"]

let run_main () = setup main_loop
