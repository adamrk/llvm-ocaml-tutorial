{
  open Core
  open Lexing
  open Ast

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }

  let echo token =
    (* printf !"token: %{sexp: token}\n" token; *)
    token
}

  let white   = [' ' '\t']+
  let newline = '\n' | '\n' | "\r\n"
  let id      = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
  let digit   = ['0'-'9']
  let frac    = '.' digit*
  let float   = digit* frac? 

  rule read =
    parse 
    | white    { read lexbuf }
    | newline  { next_line lexbuf; read lexbuf }
    | "def"    { DEF |> echo}
    | "extern" { EXTERN |> echo}
    | id       { IDENT (Lexing.lexeme lexbuf) |> echo}
    | float    { NUMBER (float_of_string (Lexing.lexeme lexbuf)) |> echo}
    | "("      { LEFT_PAREN |> echo}
    | ")"      { RIGHT_PAREN |> echo}
    | ","      { COMMA |> echo}
    | ";"      { SEMICOLON |> echo}
    | _        { KWD (Lexing.lexeme_char lexbuf 0) |> echo}
    | eof      { EOF |> echo}

  and read_comment =
    parse
    | newline { read lexbuf }
    | _       { read_comment lexbuf }
    | eof     { EOF }

