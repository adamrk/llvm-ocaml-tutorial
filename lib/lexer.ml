open! Core
open Angstrom
open Import

module Token = struct
  module T = struct
    type t =
      | Def
      | Extern
      | Ident of string
      | Number of float
      | Kwd of char
      | If
      | Then
      | Else
      | For
      | In
      | Binary
      | Unary
      | Var
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Expr = struct
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

module Prototype = struct
  type t =
    | Prototype of string * string array
    | BinOpPrototype of string * string array * int
  [@@deriving compare, sexp]

  let set_name name = function
    | Prototype (_, args) -> Prototype (name, args)
    | BinOpPrototype (_, args, prec) -> BinOpPrototype (name, args, prec)

  let get_name = function
    | Prototype (name, _) | BinOpPrototype (name, _, _) -> name
end

module Function = struct
  type t = Function of Prototype.t * Expr.t [@@deriving compare, sexp]

  let set_name name (Function (proto, expr)) =
    Function (Prototype.set_name name proto, expr)

  let get_name (Function (proto, _)) = Prototype.get_name proto
end

let get_whitespace = skip_while Char.is_whitespace

let get_def_extern_ident c =
  take_while Char.is_alphanum
  >>| fun rest ->
  match Char.to_string c ^ rest with
  | "def" -> Token.Def
  | "extern" -> Extern
  | "if" -> If
  | "then" -> Then
  | "else" -> Else
  | "for" -> For
  | "in" -> In
  | "binary" -> Binary
  | "unary" -> Unary
  | "var" -> Var
  | other -> Ident other

let get_number c =
  take_while (fun c -> Char.is_digit c || Char.equal c '.')
  >>| fun rest -> Token.Number (Float.of_string (Char.to_string c ^ rest))

let get_comment = skip_while (fun c -> Char.( <> ) c '\n')

let get_token =
  fix (fun get_token ->
      any_char
      >>= fun first_char ->
      if Char.is_alpha first_char then get_def_extern_ident first_char
      else if Char.is_digit first_char then get_number first_char
      else if Char.equal first_char '#' then get_comment *> get_token
      else return (Token.Kwd first_char) )

let get_token =
  get_whitespace *> get_token
  <* get_whitespace
  >>| (fun tok -> Some tok)
  <|> return None

let get_comma =
  get_token
  >>= function
  | Some (Token.Kwd ',') -> return () | _other -> fail "expected comma"

let binop_precedence =
  Old_hashtbl.create 10
  |> fun table ->
  List.iter [('=', 2); ('<', 10); ('+', 20); ('-', 20); ('*', 40)]
    ~f:(fun (c, prec) -> Old_hashtbl.add table c prec ) ;
  table

let precedence c =
  Option.value (Old_hashtbl.find_opt binop_precedence c) ~default:(-1)

let expect_token expected =
  get_token
  >>= function
  | None -> fail (sprintf !"expected %{sexp: Token.t} but got None" expected)
  | Some token ->
      if Token.equal token expected then return ()
      else
        fail
          (sprintf
             !"expected %{sexp: Token.t} but got %{sexp: Token.t}"
             expected token)

let expect_id =
  get_token
  >>= function
  | None -> fail "expected id but got None"
  | Some (Token.Ident id) -> return id
  | Some token ->
      fail (sprintf !"expected ident but got %{sexp: Token.t}" token)

let get_optionally to_get = option None (to_get >>| fun result -> Some result)

let get_exp =
  fix (fun (get_exp : Expr.t t) ->
      let get_args = sep_by get_comma get_exp in
      let get_var_and_init =
        expect_id
        >>= fun id ->
        get_optionally (expect_token (Token.Kwd '=') *> get_exp)
        >>| fun exp_opt -> (id, exp_opt)
      in
      let get_vars =
        sep_by1 (expect_token (Token.Kwd ',')) get_var_and_init
        >>| Array.of_list
      in
      let get_primary =
        get_whitespace *> get_token
        <* get_whitespace
        >>= function
        | Some Token.Var ->
            get_vars
            >>= fun vars ->
            expect_token Token.In *> get_exp >>| fun exp -> Expr.Var (vars, exp)
        | Some (Token.Number n) -> return (Expr.Number n)
        | Some Token.If -> (
            commit
            >>= fun () ->
            get_exp
            >>= fun condition ->
            get_token
            >>= function
            | Some Token.Then -> (
                get_exp
                >>= fun then_ ->
                get_token
                >>= function
                | Some Token.Else ->
                    get_exp
                    >>= fun else_ -> return (Expr.If (condition, then_, else_))
                | other ->
                    fail
                      (sprintf
                         !"expected else, but got %{sexp: Token.t option}"
                         other) )
            | other ->
                fail
                  (sprintf
                     !"expected then, but got %{sexp: Token.t option}"
                     other) )
        | Some (Kwd '(') -> (
            get_exp
            >>= fun exp ->
            get_token
            >>= fun last_token ->
            match last_token with
            | Some (Token.Kwd ')') -> return exp
            | other ->
                Angstrom.fail
                  (sprintf !"expected ), but got %{sexp: Token.t option}" other)
            )
        | Some (Ident id) ->
            get_token
            >>= (function
                  | Some (Token.Kwd '(') -> (
                      commit
                      >>= fun () ->
                      get_args
                      >>= fun args ->
                      get_token
                      >>= function
                      | Some (Token.Kwd ')') ->
                          return (Expr.Call (id, Array.of_list args))
                      | other ->
                          fail
                            (sprintf
                               !"expected ')' but got %{sexp: Token.t option}"
                               other) )
                  | _other -> fail "no arguments)")
            <|> return (Expr.Variable id)
        | Some Token.For -> (
            commit
            >>= fun () ->
            get_token
            >>= function
            | Some (Token.Ident id) -> (
                get_token
                >>= function
                | Some (Token.Kwd '=') -> (
                    get_exp
                    >>= fun start ->
                    get_token
                    >>= function
                    | Some (Token.Kwd ',') -> (
                        get_exp
                        >>= fun end_ ->
                        option None
                          ( get_token
                          >>= function
                          | Some (Token.Kwd ',') -> get_exp >>| fun e -> Some e
                          | _other -> fail "expected step" )
                        >>= fun step ->
                        get_token
                        >>= function
                        | Some Token.In ->
                            get_exp
                            >>| fun body ->
                            Expr.For (id, start, end_, step, body)
                        | other ->
                            fail
                              (sprintf
                                 !"expected 'in' but got %{sexp: Token.t \
                                   option}"
                                 other) )
                    | other ->
                        fail
                          (sprintf
                             !"expected ',' but got %{sexp: Token.t option}"
                             other) )
                | other ->
                    fail
                      (sprintf
                         !"expected '=' but got %{sexp: Token.t option}"
                         other) )
            | other ->
                fail
                  (sprintf
                     !"expected 'other' but got %{sexp: Token.t option}"
                     other) )
        | other ->
            fail (sprintf !"got unexpected %{sexp: Token.t option}" other)
      in
      let get_unary =
        get_token
        >>= (function
              | Some (Token.Kwd op) ->
                  if Char.(op = '(' || op = ')') then
                    fail "( and ) are not operators"
                  else get_exp >>| fun exp -> Expr.Unary (op, exp)
              | _ -> fail "not an operator")
        <|> get_primary
      in
      let rec get_rhs prec lhs =
        get_token
        >>= (function
              | Some (Token.Kwd c) ->
                  let prec_1 = precedence c in
                  if prec_1 < prec then fail "precedence too low"
                  else
                    commit
                    >>= (fun () ->
                          get_unary
                          >>= fun rhs ->
                          get_whitespace *> peek_char
                          >>= (function
                                | Some c2 ->
                                    let prec_2 = precedence c2 in
                                    if prec_1 < prec_2 then
                                      fail "got higher precedence"
                                    else return rhs
                                | _ -> return rhs)
                          <|> get_rhs (prec_1 + 1) rhs )
                    >>= fun rhs ->
                    let lhs = Expr.Binary (c, lhs, rhs) in
                    get_rhs prec lhs
              | _other -> fail "not a binop")
        <|> return lhs
      in
      get_unary >>= fun lhs -> get_rhs 0 lhs )

let get_args =
  let get_arg =
    get_token
    >>= function
    | Some (Token.Ident id) -> return id | _ -> fail "not an argument"
  in
  many get_arg

let get_prototype =
  let get_precedence =
    get_token >>| function Some (Token.Number n) -> Int.of_float n | _ -> 30
  in
  let all_args =
    get_token
    >>= function
    | Some (Token.Kwd '(') -> (
        commit
        >>= fun () ->
        get_args
        >>= fun args ->
        get_token
        >>= function
        | Some (Token.Kwd ')') -> return args
        | Some token ->
            fail (sprintf !"expected ), but found %{sexp: Token.t}" token)
        | None -> fail "expected ) not found" )
    | _ -> fail "no arguments"
  in
  get_token
  >>= function
  | Some (Token.Ident id) ->
      all_args
      >>= fun args -> return (Prototype.Prototype (id, Array.of_list args))
  | Some token -> (
      ( match token with
      | Token.Unary -> return `Unary
      | Token.Binary -> return `Binary
      | other ->
          fail
            (sprintf !"expected binary or unary, got %{sexp: Token.t}" other)
      )
      >>= fun operator_type ->
      commit
      >>= fun () ->
      get_token
      >>= function
      | Some (Token.Kwd op) -> (
          ( match token with
          | Token.Binary -> get_precedence >>| fun prec -> Some prec
          | _ -> return None )
          >>= fun precendence_opt ->
          all_args
          >>= fun args ->
          ( match (operator_type, List.length args) with
          | `Unary, 1 -> return ()
          | `Binary, 2 -> return ()
          | other ->
              fail
                (sprintf
                   !"number of arguments doesn't match: %{sexp: [`Unary | \
                     `Binary] * int}"
                   other) )
          >>| fun () ->
          let name =
            match operator_type with
            | `Unary -> "unary" ^ String.make 1 op
            | `Binary -> "binary" ^ String.make 1 op
          in
          let args = Array.of_list args in
          match operator_type with
          | `Unary -> Prototype.Prototype (name, args)
          | `Binary ->
              Prototype.BinOpPrototype
                (name, args, Option.value_exn precendence_opt ~here:[%here]) )
      | other ->
          fail
            (sprintf
               !"expected operator, but got %{sexp: Token.t option}"
               other) )
  | _ -> fail "no identifier"

let get_definition =
  get_token
  >>= function
  | Some Token.Def ->
      commit
      >>= fun () ->
      get_prototype
      >>= fun prototype ->
      get_exp >>| fun exp -> Function.Function (prototype, exp)
  | _ -> fail "not a function definition"

let get_extern =
  get_token
  >>= function
  | Some Token.Extern -> commit >>= fun () -> get_prototype
  | _ -> fail "not an extern definition"

let get_toplevel =
  get_exp
  >>| fun expr -> Function.Function (Prototype.Prototype ("", [||]), expr)

let print_exp s =
  let result = parse_string get_exp s in
  printf !"%{sexp: (Expr.t, string) Result.t}" result

let%expect_test "binops" =
  print_exp "x + y" ;
  [%expect {| (Ok (Binary + (Variable x) (Variable y))) |}] ;
  print_exp "x + y * 5" ;
  [%expect
    {| (Ok (Binary + (Variable x) (Binary * (Variable y) (Number 5)))) |}] ;
  print_exp "!x * z" ;
  [%expect {| (Ok (Unary ! (Binary * (Variable x) (Variable z)))) |}] ;
  print_exp "x + !y * z" ;
  [%expect
    {| (Ok (Binary + (Variable x) (Unary ! (Binary * (Variable y) (Variable z))))) |}]
