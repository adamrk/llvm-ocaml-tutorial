open! Core

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Parse and print kaleidoscope"
    [%map_open
      let file = flag "file" (optional string) ~doc:"FILE read input from file" in
      fun () -> Kaleidoscope_lib.Toplevel.main (match file with
    | None -> `Stdin
    | Some file -> `File file)]
  |> Command.run
