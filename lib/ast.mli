open Core

type token =
  | DEF
  | EXTERN
  | IF
  | THEN
  | ELSE
  | FOR
  | IN
  | BINARY
  | UNARY
  | VAR
  | IDENT of string
  | NUMBER of float
  | KWD of char
  | LEFT_PAREN
  | RIGHT_PAREN
  | EQUALS
  | COMMA
  | SEMICOLON
  | EOF
[@@deriving sexp]

type proto =
  | Prototype of string * string list
  | BinOpPrototype of string * string list * int
[@@deriving sexp]

module Expr : sig
  module No_binop : sig
    type t =
      | Number of float
      | Variable of string
      | Unary of char * t
      | Bin_list of t * (char * int * t) list
      | Call of string * t list
      | If of t * t * t
      | For of string * t * t * t option * t
      | Var of (string * t option) list * t
    [@@deriving sexp]

    type func = Function of proto * t [@@deriving sexp]
  end

  type t =
    | Number of float
    | Variable of string
    | Unary of char * t
    | Binary of char * t * t
    | Call of string * t list
    | If of t * t * t
    | For of string * t * t * t option * t
    | Var of (string * t option) list * t
  [@@deriving sexp]

  val of_no_binop : No_binop.t -> t
end

type func = Function of proto * Expr.t [@@deriving sexp]

val func_of_no_binop_func : Expr.No_binop.func -> func

val set_func_name : string -> func -> func

val binop_precedence : (char, int) Hashtbl.t
