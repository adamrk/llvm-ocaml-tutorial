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

%start <Ast.expr list> prog
%start <Ast.expr> expr
%%

prog:
  | es = separated_list(COMMA, expr); EOF  { es }
  ;
  
primary:
  | f = NUMBER { Ast.Number f }
  | LEFT_PAREN; e = expr; RIGHT_PAREN { e }
  | id = IDENT; args = option(args) 
  { match args with
    | None -> Ast.Variable id
    | Some args -> Ast.Call (id, Array.of_list args)
  }
  ;

args: LEFT_PAREN; args = separated_list(COMMA, expr); RIGHT_PAREN { args };

expr: lhs = primary { lhs };
