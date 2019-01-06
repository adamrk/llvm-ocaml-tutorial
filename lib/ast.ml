open Core

type token =
  | DEF
  | EXTERN
  | IDENT of string
  | NUMBER of float
  | KWD of char
  | LEFT_PAREN
  | RIGHT_PAREN
  | COMMA
  | SEMICOLON
  | EOF
[@@deriving sexp]

type expr =
  | Number of float
  | Variable of string
  | Binary of char * expr * expr
  | Call of string * expr array
[@@deriving sexp]

type proto = Prototype of string * string array [@@deriving sexp]

type func = Function of proto * expr [@@deriving sexp]
