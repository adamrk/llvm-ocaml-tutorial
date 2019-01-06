open Core

let parse_and_print lexbuf =
  let expr = Menhir_parser.single_expr Ocamllexer.read lexbuf in 
  printf !"first time: %{sexp: Ast.expr}\n" expr;
  let expr = Menhir_parser.single_expr Ocamllexer.read lexbuf in 
  printf !"second time: %{sexp: Ast.expr}\n" expr

let rec get_full_expr accum =
  Out_channel.(flush stdout);
  let new_line = In_channel.(input_line_exn stdin) in
  match String.mem new_line ';' with
  | true -> List.rev (new_line :: accum) |> String.concat ~sep:"\n"
  | false -> get_full_expr (new_line :: accum)

let rec run_main () =
  let input = get_full_expr [] in
  parse_and_print (Lexing.from_string input);
  run_main ()
