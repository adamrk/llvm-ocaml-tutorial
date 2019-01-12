open Core

type token =
  | DEF
  | EXTERN
  | IF
  | THEN
  | ELSE
  | FOR
  | IN
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

type proto = Prototype of string * string list [@@deriving sexp]

module Expr = struct
  module No_binop = struct
    type t =
      | Number of float
      | Variable of string
      | Bin_list of t * (char * int * t) list
      | Call of string * t list
      | If of t * t * t
      | For of string * t * t * t option * t
    [@@deriving sexp]

    type func = Function of proto * t [@@deriving sexp]

    let rec reduce first rest =
      match rest with
      | (first_op, first_prec, _) :: tail -> (
          let (_highest_op : char), (_ : int), index =
            List.foldi tail ~init:(first_op, first_prec, 0)
              ~f:(fun new_inx
                 (highest_op, highest_prec, inx)
                 (new_op, new_prec, _new_expr)
                 ->
                if Int.( > ) new_prec highest_prec then
                  (new_op, new_prec, new_inx + 1)
                else (highest_op, highest_prec, inx) )
          in
          match index with
          | 0 ->
              let to_reduce = List.hd_exn rest in
              let expr = Bin_list (first, [to_reduce]) in
              reduce expr (List.tl_exn rest)
          | n ->
              let to_reduce = List.nth_exn rest n in
              let prev_op, prev_prec, prev_expr = List.nth_exn rest (n - 1) in
              let new_expr =
                (prev_op, prev_prec, Bin_list (prev_expr, [to_reduce]))
              in
              reduce first
                (List.take rest (n - 1) @ (new_expr :: List.drop rest (n + 1)))
          )
      | [] -> first
  end

  type t =
    | Number of float
    | Variable of string
    | Binary of char * t * t
    | Call of string * t list
    | If of t * t * t
    | For of string * t * t * t option * t
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

type func = Function of proto * Expr.t [@@deriving sexp]

let func_of_no_binop_func (Expr.No_binop.Function (proto, body)) =
  Function (proto, Expr.of_no_binop body)

let set_func_name name (Function (proto, body)) =
  let new_proto =
    match proto with Prototype ((_name : string), args) ->
      Prototype (name, args)
  in
  Function (new_proto, body)

let binop_precedence : (char, int) Hashtbl.t = Hashtbl.create (module Char)
