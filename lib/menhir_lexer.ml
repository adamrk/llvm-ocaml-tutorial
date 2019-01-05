type token =
  | DEF
  | EXTERN
  | IDENT of string
  | NUMBER of float
  | KWD of char
  | EOF
