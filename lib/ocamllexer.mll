{
  open Lexing
  open Ast

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
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
    | id       { IDENT (Lexing.lexeme lexbuf) }
    | "def"    { DEF }
    | "extern" { EXTERN }
    | float    { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
    | "("      { LEFT_PAREN }
    | ")"      { RIGHT_PAREN }
    | ","      { COMMA }
    | ";"      { SEMICOLON }
    | _        { KWD (Lexing.lexeme_char lexbuf 0) }
    | eof      { EOF }

  and read_comment =
    parse
    | newline { read lexbuf }
    | _       { read_comment lexbuf }
    | eof     { EOF }

