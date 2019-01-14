%token DEF
%token EXTERN
%token IF
%token THEN
%token ELSE
%token FOR
%token IN
%token BINARY
%token UNARY
%token VAR
%token <string> IDENT
%token <float> NUMBER
%token <char> KWD
%token EQUALS
%token LEFT_PAREN
%token RIGHT_PAREN
%token COMMA
%token SEMICOLON
%token EOF

%{ 
  open Core
  open Ast
  let precedence c = 
    match Hashtbl.find binop_precedence c with 
    | None -> -1
    | Some p -> p
%}

%start <Ast.Expr.No_binop.t list> prog
%start < [`Expr of Ast.Expr.No_binop.func 
         | `Extern of Ast.proto 
         | `Def of Ast.Expr.No_binop.func 
         | `Eof ]> toplevel
%%

prog:
  | es = separated_list(SEMICOLON, expr); EOF  { es }
  ;

toplevel:
  | e = expr; SEMICOLON { `Expr (Expr.No_binop.Function (Prototype ("", []), e)) }
  | e = extern; SEMICOLON { `Extern e } 
  | d = definition; SEMICOLON { `Def d }
  | EOF { `Eof }
  ;
  
primary:
  | f = NUMBER 
    { Expr.No_binop.Number f }
  | LEFT_PAREN; e = expr; RIGHT_PAREN { e }
  | id = IDENT; args = delimited(LEFT_PAREN, separated_list(COMMA, expr), RIGHT_PAREN) 
    { (* printf !"got id %s\n" id; *) Expr.No_binop.Call (id, args) }
  | id = IDENT; { Expr.No_binop.Variable id }
  ;

block:
  | IF; c = expr; THEN; t = expr; ELSE; e = expr { Expr.No_binop.If (c, t, e) }
  | FOR; id = IDENT; EQUALS; start = expr; COMMA; end_ = expr; 
    step = option(COMMA; e = expr { e }); IN; body = expr 
    { Expr.No_binop.For (id, start, end_, step, body) }
  | VAR; vars = separated_nonempty_list(COMMA, var); IN; body = expr
    { Expr.No_binop.Var (vars, body) }
  ;

var: name = IDENT; e = option(EQUALS; e = expr { e }) { (name, e) }

unary: 
  | op = operator; operand = expr_wo_rhs { Expr.No_binop.Unary (op, operand) }
  | e = primary                          { e }
  ;

rhs: 
  | op = operator; unop = operator; e = expr_wo_rhs 
    { (op, precedence op, Expr.No_binop.Unary (unop, e)) }
  | op = operator; e = primary
    { (op, precedence op, e ) }
  ;

expr: 
  | lhs = unary; rest = list(rhs) 
  { (* printf !"lhs: %{sexp:Expr.No_binop.t}, rhs: %{sexp: (char * int * Expr.No_binop.t) list}\n" 
     lhs rest; *)
    match rest with
    | [] -> lhs
    | _  -> Expr.No_binop.Bin_list (lhs, rest) 
  }
  | e = block { e }
  ;

expr_wo_rhs: e = unary { e }

prototype:
  | name = IDENT; args = delimited(LEFT_PAREN, list(IDENT), RIGHT_PAREN)
    { Prototype (name, args) }
  | kind = operator_kind; op = operator; prec = precedence; 
    args = delimited(LEFT_PAREN, list(IDENT), RIGHT_PAREN) 
    { match kind with
      | `Binary -> 
          if Int.(<>) (List.length args) 2 
          then raise_s 
            [%message "binary operator doesn't have 2 arguments" (args : string list)]
          else 
            Ast.BinOpPrototype ("binary" ^ String.of_char op, args, prec)
      | `Unary ->
          if Int.(<>) (List.length args) 1 
          then raise_s 
            [%message "unary operator doesn't have 1 argument" (args : string list)]
          else 
            Ast.Prototype ("unary" ^ String.of_char op, args)
    }
  ;

operator:
  | op = KWD { op }
  | EQUALS   { '=' }
  ;

operator_kind:
  | UNARY  { `Unary }
  | BINARY { `Binary }
  ;

precedence:
  | n = option(NUMBER) { Int.of_float (Option.value n ~default:30.0) }
  ;

definition:
  | DEF; proto = prototype; body = expr { Expr.No_binop.Function (proto, body) }
  ;

extern: EXTERN; proto = prototype { proto }

