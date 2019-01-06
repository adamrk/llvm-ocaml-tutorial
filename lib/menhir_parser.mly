%token DEF
%token EXTERN
%token <string> IDENT
%token <float> NUMBER
%token <char> KWD
%token LEFT_PAREN
%token RIGHT_PAREN
%token COMMA
%token SEMICOLON
%token EOF

%{ 
  open Core
  open Ast
  let _precedence c = 
    match Hashtbl.find binop_precedence c with 
    | None -> -1
    | Some p -> p
%}

%start <Ast.Expr.No_binop.t list> prog
%start <Ast.Expr.No_binop.t> single_expr
%%

prog:
  | es = separated_list(SEMICOLON, expr); EOF  { es }
  ;
  
primary:
  | f = NUMBER 
    { let count = !counts in counts := count + 1; printf "%d\n" count; Expr.No_binop.Number (f +. float_of_int count) }
  | LEFT_PAREN; e = expr; RIGHT_PAREN { e }
  | id = IDENT; args = delimited(LEFT_PAREN, separated_list(COMMA, expr), RIGHT_PAREN) 
    { printf !"got id %s\n" id; Expr.No_binop.Call (id, args) }
  | id = IDENT; { Expr.No_binop.Variable id }
  ;

rhs: op = KWD; expr = primary { (op, _precedence op, expr) }

expr: 
  | lhs = primary; rest = list(rhs) 
  { printf !"lhs: %{sexp:Expr.No_binop.t}, rhs: %{sexp: (char * int * Expr.No_binop.t) list}\n" 
     lhs rest;
    match rest with
    | [] -> lhs
    | _  -> Expr.No_binop.Bin_list (lhs, rest) 
  }

single_expr:
  | e = expr; SEMICOLON { e }
