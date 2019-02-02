{
  open Ast
}

  let white   = [' ' '\t' '\n' '\r']+
  let newline = '\n' | '\r' | "\r\n"
  let id      = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
  let digit   = ['0'-'9']
  let frac    = '.' digit*
  let float   = digit* frac? 

  rule read =
    parse 
    (* skip whitespace *)
    | white    { read lexbuf }
    | newline  { read lexbuf }
    | "def"    { DEF }
    | "extern" { EXTERN }
    | "if"     { IF }
    | "then"   { THEN }
    | "else"   { ELSE }
    | "for"    { FOR }
    | "in"     { IN }
    | "binary" { BINARY }
    | "unary"  { UNARY }
    | "var"    { VAR }
    | id       { IDENT (Lexing.lexeme lexbuf) }
    | float    { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
    | ":="     { ASSIGN }
    | "("      { LEFT_PAREN }
    | ")"      { RIGHT_PAREN }
    | ","      { COMMA }
    | ";"      { SEMICOLON }
    (* '#' marks the beginning of a comment *)
    | "#"      { read_comment lexbuf }
    | _        { KWD (Lexing.lexeme_char lexbuf 0) }
    | eof      { EOF }

  and read_comment =
    parse
    (* comment continues until newline *)
    | newline { read lexbuf }
    | _       { read_comment lexbuf }
    | eof     { EOF }

