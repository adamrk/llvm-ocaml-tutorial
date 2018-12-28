open! Core
open Import

module Expr : sig
  type t =
    | Number of float
    | Variable of string
    | Binary of char * t * t
    | Unary of char * t
    | Call of string * t array
    | If of t * t * t
    | For of string * t * t * t option * t
    | Var of (string * t option) array * t
  [@@deriving compare, sexp]
end

module Prototype : sig
  type t =
    | Prototype of string * string array
    | BinOpPrototype of string * string array * int
  [@@deriving compare, sexp]
end

module Function : sig
  type t = Function of Prototype.t * Expr.t [@@deriving compare, sexp]

  val set_name : string -> t -> t

  val get_name : t -> string
end

val get_definition : Function.t Angstrom.t

val get_extern : Prototype.t Angstrom.t

val get_toplevel : Function.t Angstrom.t

val binop_precedence : (char, int) Old_hashtbl.t 
