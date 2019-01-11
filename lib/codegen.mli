open! Core

val the_module : Llvm.llmodule

val codegen_proto : Ast.proto -> Llvm.llvalue

val codegen_func : [`Function] Llvm.PassManager.t -> Ast.func -> Llvm.llvalue
