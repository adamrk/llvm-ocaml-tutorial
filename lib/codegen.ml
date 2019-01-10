open Core

let context = Llvm.global_context ()

let the_module = Llvm.create_module context "my cool jit"

let builder = Llvm.builder context

let named_values : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create (module String) 

let double_type = Llvm.double_type context

(*
let _create_entry_block_alloca the_function var_name =
  let builder =
    Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function))
  in
  Llvm.build_alloca double_type var_name builder
  *)

let rec codegen_expr = function
  (*
  | Lexer.Expr.Var (var_names, body) ->
      let old_bindings = ref [] in
      let the_function = Llvm.block_parent (Llvm.insertion_block builder) in
      Array.iter var_names ~f:(fun (var_name, init) ->
          let init_val =
            match init with
            | Some init -> codegen_expr init
            | None -> Llvm.const_float double_type 0.0
          in
          let alloca = create_entry_block_alloca the_function var_name in
          Llvm.build_store init_val alloca builder |> ignore ;
          ( match Old_hashtbl.find_opt named_values var_name with
          | None -> ()
          | Some old_value ->
              old_bindings := (var_name, old_value) :: !old_bindings ) ;
          Old_hashtbl.add named_values var_name alloca ) ;
      let body_val = codegen_expr body in
      List.iter !old_bindings ~f:(fun (var_name, old_value) ->
          Old_hashtbl.add named_values var_name old_value ) ;
      body_val
      *)
  | Ast.Expr.Number n -> Llvm.const_float double_type n
  | Ast.Expr.Variable name -> (
    match Hashtbl.find named_values name with
    | None -> raise_s [%message "unkown variable name" (name : string)]
    | Some v -> v )
  | Ast.Expr.Binary (op, lhs, rhs) -> (
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      (match op with
      (*
      | '=' ->
          let name =
            match lhs with
            | Lexer.Expr.Variable name -> name
            | _ -> raise_s [%message "destination of '=' must be a variable"]
          in
          let val_ = codegen_expr rhs in
          let variable =
            try Old_hashtbl.find named_values name with _ ->
              raise_s [%message "unknown variable name" (name : string)]
          in
          Llvm.build_store val_ variable builder |> ignore ;
          val_
          *)
      | '+' -> Llvm.build_fadd lhs_val rhs_val "addtmp" builder
      | '-' -> Llvm.build_fsub lhs_val rhs_val "subtmp" builder
      | '*' -> Llvm.build_fmul lhs_val rhs_val "multmp" builder
      | '<' ->
          let i =
            Llvm.build_fcmp Llvm.Fcmp.Ult lhs_val rhs_val "cmptmp" builder
          in
          Llvm.build_uitofp i double_type "booltmp" builder
      | _ ->
          raise_s [%message "operator not recognized"]))
          (*
          let callee = "binary" ^ String.make 1 op in
          let callee =
            match Llvm.lookup_function callee the_module with
            | Some callee -> callee
            | None -> raise_s [%message "unrecognized binop" (op : char)]
          in
          Llvm.build_call callee [|lhs_val; rhs_val|] "binop" builder )
  *)
  | Ast.Expr.Call (callee_name, args) ->
      let callee =
        match Llvm.lookup_function callee_name the_module with
        | Some callee -> callee
        | None ->
            raise_s [%message "undefined function" (callee_name : string)]
      in
      if Int.( = ) (Array.length (Llvm.params callee)) (List.length args) then
        ()
      else
        raise_s
          [%message "incorrect number of arguments" (callee_name : string)] ;
      let args = Array.map (Array.of_list args) ~f:codegen_expr in
      Llvm.build_call callee args "calltmp" builder
      (*
  | Lexer.Expr.If (condition, then_, else_) ->
      let cond = codegen_expr condition in
      let zero = Llvm.const_float double_type 0.0 in
      let cond_val =
        Llvm.build_fcmp Llvm.Fcmp.One cond zero "ifcond" builder
      in
      let start_bb = Llvm.insertion_block builder in
      let the_function = Llvm.block_parent start_bb in
      let then_bb = Llvm.append_block context "then" the_function in
      Llvm.position_at_end then_bb builder ;
      let then_val = codegen_expr then_ in
      (* last codegen might have changed the insertion block *)
      let new_then_bb = Llvm.insertion_block builder in
      let else_bb = Llvm.append_block context "else" the_function in
      Llvm.position_at_end else_bb builder ;
      let else_val = codegen_expr else_ in
      (* last codegen might have changed the insertion block *)
      let new_else_bb = Llvm.insertion_block builder in
      let merge_bb = Llvm.append_block context "ifcont" the_function in
      Llvm.position_at_end merge_bb builder ;
      let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
      let phi = Llvm.build_phi incoming "iftmp" builder in
      Llvm.position_at_end start_bb builder ;
      Llvm.build_cond_br cond_val then_bb else_bb builder |> ignore ;
      Llvm.position_at_end new_then_bb builder ;
      Llvm.build_br merge_bb builder |> ignore ;
      Llvm.position_at_end new_else_bb builder ;
      Llvm.build_br merge_bb builder |> ignore ;
      Llvm.position_at_end merge_bb builder ;
      phi
  | Lexer.Expr.For (var_name, start, end_, step, body) ->
      let the_function = Llvm.block_parent (Llvm.insertion_block builder) in
      let alloca = create_entry_block_alloca the_function var_name in
      let start_val = codegen_expr start in
      Llvm.build_store start_val alloca builder |> ignore ;
      let loop_bb = Llvm.append_block context "loop" the_function in
      (* explicit fall through to loop_bb *)
      Llvm.build_br loop_bb builder |> ignore ;
      Llvm.position_at_end loop_bb builder ;
      let old_val =
        try Some (Old_hashtbl.find named_values var_name) with _ -> None
      in
      Old_hashtbl.add named_values var_name alloca ;
      codegen_expr body |> ignore ;
      let step_val =
        match step with
        | Some step -> codegen_expr step
        | None -> Llvm.const_float double_type 1.0
      in
      let end_cond = codegen_expr end_ in
      let cur_var = Llvm.build_load alloca var_name builder in
      let next_var = Llvm.build_add cur_var step_val "nextvar" builder in
      Llvm.build_store next_var alloca builder |> ignore ;
      let zero = Llvm.const_float double_type 0.0 in
      let end_cond =
        Llvm.build_fcmp Llvm.Fcmp.One end_cond zero "loopcond" builder
      in
      let after_bb = Llvm.append_block context "afterloop" the_function in
      Llvm.build_cond_br end_cond loop_bb after_bb builder |> ignore ;
      Llvm.position_at_end after_bb builder ;
      ( match old_val with
      | Some old_val -> Old_hashtbl.add named_values var_name old_val
      | None -> () ) ;
      Llvm.const_null double_type
  | Lexer.Expr.Unary (op, operand) ->
      let operand = codegen_expr operand in
      let callee = "unary" ^ String.make 1 op in
      let callee =
        match Llvm.lookup_function callee the_module with
        | Some callee -> callee
        | None -> raise_s [%message "unknown unary operator" (op : char)]
      in
      Llvm.build_call callee [|operand|] "unop" builder
      *)

let codegen_proto_existing = function
  | Ast.Prototype (name, args) ->
      let doubles = Array.create ~len:(List.length args) double_type in
      let ft = Llvm.function_type double_type doubles in
      let f, existing =
        match Llvm.lookup_function name the_module with
        | None -> (Llvm.declare_function name ft the_module, `Existing)
        | Some f ->
            if Int.(Array.length (Llvm.basic_blocks f) = 0) then ()
            else raise_s [%message "redefinition of function" (name : string)] ;
            if Int.(Array.length (Llvm.params f) = List.length args) then ()
            else
              raise_s
                [%message
                  "redefinition of function with a different number of args"
                    (name : string)] ;
            (f, `Not_existing)
      in
      Array.iteri (Llvm.params f) ~f:(fun i a ->
          let name = List.nth_exn args i in
          Llvm.set_value_name name a ;
          Hashtbl.add_exn named_values ~key:name ~data:a; ) ;
      (f, existing)

      (*
let create_argument_allocas the_function proto =
  let args =
    match proto with
    | Lexer.Prototype.Prototype (_, args)
     |Lexer.Prototype.BinOpPrototype (_, args, _) ->
        args
  in
  Array.iteri (Llvm.params the_function) ~f:(fun i ai ->
      let var_name = args.(i) in
      let alloca = create_entry_block_alloca the_function var_name in
      Llvm.build_store ai alloca builder |> ignore ;
      Old_hashtbl.add named_values var_name alloca )
  *)

let codegen_func = function
  | Ast.Function (proto, body) -> (
      Hashtbl.clear named_values ;
      let the_function, existing = codegen_proto_existing proto in
      (* install an operator *)
      (*
      ( match proto with
      | Lexer.Prototype.BinOpPrototype (name, _args, prec) ->
          let op = name.[String.length name - 1] in
          Old_hashtbl.add Lexer.binop_precedence op prec
      | _ -> () ) ;
      *)
      let bb = Llvm.append_block context "entry" the_function in
      Llvm.position_at_end bb builder ;
      try
       (* create_argument_allocas the_function proto ; *)
        let return_val = codegen_expr body in
        let _ : Llvm.llvalue = Llvm.build_ret return_val builder in
        ( match Llvm_analysis.verify_function the_function with
        | true -> ()
        | false ->
            printf "invalid function generated\n%s\n"
              (Llvm.string_of_llvalue the_function) ;
            Llvm_analysis.assert_valid_function the_function ) ;
        (* optimize!! *)
        (* let _ = Llvm.PassManager.run_function the_function the_fpm in *)
        the_function
      with e ->
        ( match existing with
        | `Not_existing -> Llvm.delete_function the_function
        | `Existing ->
            Array.iter (Llvm.basic_blocks the_function) ~f:Llvm.delete_block ) ;
        raise e )

let codegen_proto proto = codegen_proto_existing proto |> fst
