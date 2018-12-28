open! Core
open Async

let lex_loop_command =
  Command.async ~summary:"run the lexer loop"
    (let open Command.Let_syntax in
    let%map_open () = Command.Param.return () 
    in
    fun () -> Kaleidoscope_lib.Toplevel.run_main ())

let () = Command.run lex_loop_command
