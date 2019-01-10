open! Core

val the_module : Llvm.llmodule

val codegen_expr : Ast.Expr.t -> Llvm.llvalue

val codegen_proto : Ast.proto -> Llvm.llvalue

val codegen_func : Ast.func -> Llvm.llvalue
