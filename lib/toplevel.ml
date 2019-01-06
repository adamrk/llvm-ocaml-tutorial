open Core

let parse_and_print lexbuf =
  let expr = Menhir_parser.single_expr Ocamllexer.read lexbuf in 
  printf !"%{sexp: Ast.Expr.t}\n" (Ast.Expr.of_no_binop expr)

let rec get_full_expr accum =
  Out_channel.(flush stdout);
  let new_line = In_channel.(input_line_exn stdin) in
  match String.mem new_line ';' with
  | true -> List.rev (new_line :: accum) |> String.concat ~sep:"\n"
  | false -> get_full_expr (new_line :: accum)

let run_main () =
  let rec loop () =
  let input = get_full_expr [] in
  printf !"got input %s\n" input;
  parse_and_print (Lexing.from_string input);
  loop ()
  in
  Hashtbl.add_exn Ast.binop_precedence ~key:'<' ~data:10;
  Hashtbl.add_exn Ast.binop_precedence ~key:'+' ~data:20;
  Hashtbl.add_exn Ast.binop_precedence ~key:'-' ~data:20;
  Hashtbl.add_exn Ast.binop_precedence ~key:'*' ~data:40;
  loop ()
