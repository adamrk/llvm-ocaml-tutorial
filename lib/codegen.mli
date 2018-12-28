open! Core

val the_module : Llvm.llmodule

val codegen_expr : Lexer.Expr.t -> Llvm.llvalue

val codegen_proto : Lexer.Prototype.t -> Llvm.llvalue

val codegen_func :
  [`Function] Llvm.PassManager.t -> Lexer.Function.t -> Llvm.llvalue
