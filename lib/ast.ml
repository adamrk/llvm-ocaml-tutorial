open Core

type token =
  (* commands *)
  | DEF
  | EXTERN
  (* control *)
  | IF
  | THEN
  | ELSE
  | FOR
  | IN
  (* operators *)
  | BINARY
  | UNARY
  (* var definition *)
  | VAR
  (* primary *)
  | IDENT of string
  | NUMBER of float
  (* unknown *)
  | KWD of char
  (* special chars *)
  | LEFT_PAREN
  | RIGHT_PAREN
  | EQUALS
  | COMMA
  | SEMICOLON
  (* end of file *)
  | EOF
[@@deriving sexp]

(* proto - This type represents the "prototype" for a function, which captures
 * its name, and its argument names (thus implicitly the number of arguments the
 * function takes). *)
type proto =
  | Prototype of string * string list
  | BinOpPrototype of string * string list * int
[@@deriving sexp]

module Expr = struct
  module No_binop = struct
    (* base type for expressions before we've properly associated binops *)
    type t =
      (* variant for numeric literals like "1.0" *)
      | Number of float
      (* variant for referencing a variable, like "a". *)
      | Variable of string
      (* variant for a unary operator. *)
      | Unary of char * t
      (* variant for a sequence binary operators, they still need to be 
       * associated based operator precedence. *)
      | Bin_list of t * (char * int * t) list
      (* variant for function calls. *)
      | Call of string * t list
      (* variant for if/then/else. *)
      | If of t * t * t
      (* variant for for/in. *)
      | For of string * t * t * t option * t
      (* variant for var/in *)
      | Var of (string * t option) list * t
    [@@deriving sexp]

    (* func - This type represents a function definition itself (still needing
     * association of binops).  *)
    type func = Function of proto * t [@@deriving sexp]

    (* group the two highest terms joined by the higest precedence operator 
     * into a single term and then recurse until there is one term. *)
    let rec reduce first rest =
      match rest with
      | (first_op, first_prec, _) :: tail -> (
          let index =
            (* search for the index of the operator with highest precedence *)
            List.foldi tail ~init:(first_op, first_prec, 0)
              ~f:(fun new_inx
                 (highest_op, highest_prec, inx)
                 (new_op, new_prec, _new_expr)
                 ->
                if Int.( > ) new_prec highest_prec then
                  (new_op, new_prec, new_inx + 1)
                else (highest_op, highest_prec, inx) )
            |> fun (_, _, index) -> index
          in
          match index with
          (* if the first operator has precedence, combine [first] and [rest[0]]
           * into new [first] and set [rest] to [tail rest]. *)
          | 0 ->
              let to_reduce = List.hd_exn rest in
              let expr = Bin_list (first, [to_reduce]) in
              reduce expr (List.tl_exn rest)
          (* if it's index n > 0 then combine the terms at index [n] and [n-1]
           * into the new [rest]. *)
          | n ->
              let to_reduce = List.nth_exn rest n in
              let prev_op, prev_prec, prev_expr = List.nth_exn rest (n - 1) in
              let new_expr =
                (prev_op, prev_prec, Bin_list (prev_expr, [to_reduce]))
              in
              reduce first
                (List.take rest (n - 1) @ (new_expr :: List.drop rest (n + 1)))
          )
      (* once there's only one term left we're done *)
      | [] -> first
  end

  type t =
    (* variant for numeric literals like "1.0" *)
    | Number of float
    (* variant for referencing a variable, like "a". *)
    | Variable of string
    (* variant for a unary operator. *)
    | Unary of char * t
    (* variant for a binary operators. *)
    | Binary of char * t * t
    (* variant for function calls. *)
    | Call of string * t list
    (* variant for if/then/else. *)
    | If of t * t * t
    (* variant for for/in. *)
    | For of string * t * t * t option * t
    (* variant for var/in *)
    | Var of (string * t option) list * t
  [@@deriving sexp]

  let rec of_no_binop = function
    | No_binop.Number f -> Number f
    | No_binop.Variable x -> Variable x
    | No_binop.Call (f, args) -> Call (f, List.map args ~f:of_no_binop)
    | No_binop.If (if_, then_, else_) ->
        If (of_no_binop if_, of_no_binop then_, of_no_binop else_)
    | No_binop.For (id, start, end_, step, body) ->
        For
          ( id
          , of_no_binop start
          , of_no_binop end_
          , Option.map step ~f:of_no_binop
          , of_no_binop body )
    | No_binop.Unary (c, t) -> Unary (c, of_no_binop t)
    | No_binop.Var (vars, body) ->
        Var
          ( List.map vars ~f:(fun (name, expr) ->
                (name, Option.map expr ~f:of_no_binop) )
          , of_no_binop body )
    | No_binop.Bin_list (first, []) -> of_no_binop first
    | No_binop.Bin_list (first, [(op, _prec, second)]) ->
        Binary (op, of_no_binop first, of_no_binop second)
    | No_binop.Bin_list (first, rest) ->
        of_no_binop (No_binop.reduce first rest)

  let%expect_test _ =
    let no_binop =
      No_binop.Bin_list
        ( No_binop.Variable "x"
        , [('*', 40, No_binop.Variable "y"); ('+', 20, No_binop.Variable "z")]
        )
    in
    printf !"%{sexp: t}" (of_no_binop no_binop) ;
    [%expect
      {| (Binary + (Binary * (Variable x) (Variable y)) (Variable z)) |}] ;
    let no_binop =
      No_binop.Bin_list
        ( No_binop.Variable "x"
        , [ ('*', 40, No_binop.Variable "y")
          ; ('+', 20, No_binop.Variable "z")
          ; ('*', 40, No_binop.Variable "w") ] )
    in
    printf !"%{sexp: t}" (of_no_binop no_binop) ;
    [%expect
      {| 
      (Binary + (Binary * (Variable x) (Variable y)) 
       (Binary * (Variable z) (Variable w))) 
    |}]
end

(* func - This type represents a function definition itself. *)
type func = Function of proto * Expr.t [@@deriving sexp]

let func_of_no_binop_func (Expr.No_binop.Function (proto, body)) =
  Function (proto, Expr.of_no_binop body)

let set_func_name name (Function (proto, body)) =
  let new_proto =
    match proto with
    | Prototype ((_name : string), args) -> Prototype (name, args)
    | BinOpPrototype ((_name : string), args, prec) ->
        BinOpPrototype (name, args, prec)
  in
  Function (new_proto, body)

(* this holds the precedence for each binary operator that is defined. It can
 * be mutated if new binops are defined *)
let binop_precedence : (char, int) Hashtbl.t = Hashtbl.create (module Char)
