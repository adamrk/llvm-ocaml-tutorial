open! Core

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Parse and print kaleidoscope"
    [%map_open
      let () = Command.Param.return () in
      fun () -> Kaleidoscope_lib.Toplevel.run_main ()]
  |> Command.run
