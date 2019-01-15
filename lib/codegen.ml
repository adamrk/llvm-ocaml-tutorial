open Core

let context = Llvm.global_context ()

let the_module = Llvm.create_module context "my cool jit"

let builder = Llvm.builder context

let named_values : (string, Llvm.llvalue) Hashtbl.t =
  Hashtbl.create (module String)

let double_type = Llvm.double_type context

(* Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. *)
let create_entry_block_alloca the_function var_name =
  let builder =
    Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function))
  in
  Llvm.build_alloca double_type var_name builder

let rec codegen_expr = function
  | Ast.Expr.Var (var_names, body) ->
      let old_bindings = ref [] in
      let the_function = Llvm.block_parent (Llvm.insertion_block builder) in
      (* Register all variables and emit their initializer. *)
      List.iter var_names ~f:(fun (var_name, init) ->
          (* Emit the initializer before adding the variable to scope, this
           * prevents the initializer from referencing the variable itself, and
           * permits stuff like this:
           *   var a = 1 in
           *     var a = a in ...   # refers to outer 'a'. *)
          let init_val =
            match init with
            | Some init -> codegen_expr init
            (* If not specified, use 0.0. *)
            | None -> Llvm.const_float double_type 0.0
          in
          let alloca = create_entry_block_alloca the_function var_name in
          Llvm.build_store init_val alloca builder |> ignore ;
          (* Remember the old variable binding so that we can restore the binding
           * when we unrecurse. *)
          ( match Hashtbl.find named_values var_name with
          | None -> ()
          | Some old_value ->
              old_bindings := (var_name, old_value) :: !old_bindings ) ;
          (* Remember this binding. *)
          Hashtbl.set named_values ~key:var_name ~data:alloca ) ;
      (* Codegen the body, now that all vars are in scope. *)
      let body_val = codegen_expr body in
      (* Pop all our variables from scope. *)
      List.iter !old_bindings ~f:(fun (var_name, old_value) ->
          Hashtbl.set named_values ~key:var_name ~data:old_value ) ;
      (* Return the body computation. *)
      body_val
  | Ast.Expr.Number n -> Llvm.const_float double_type n
  | Ast.Expr.Variable name -> (
    match Hashtbl.find named_values name with
    | None -> raise_s [%message "unkown variable name" (name : string)]
    (* Load the value *)
    | Some v -> Llvm.build_load v name builder )
  | Ast.Expr.Binary ('=', lhs, rhs) ->
      (* Special case '=' because we don't want to emit the LHS as an
       * expression. *)
      let name =
        match lhs with
        | Ast.Expr.Variable name -> name
        | _ -> raise_s [%message "destination of '=' must be a variable"]
      in
      (* Codegen the rhs. *)
      let val_ = codegen_expr rhs in
      (* Lookup the name. *)
      let variable =
        match Hashtbl.find named_values name with
        | None -> raise_s [%message "unknown variable name" (name : string)]
        | Some var -> var
      in
      Llvm.build_store val_ variable builder |> ignore ;
      val_
  | Ast.Expr.Binary (op, lhs, rhs) -> (
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      match op with
      | '+' -> Llvm.build_fadd lhs_val rhs_val "addtmp" builder
      | '-' -> Llvm.build_fsub lhs_val rhs_val "subtmp" builder
      | '*' -> Llvm.build_fmul lhs_val rhs_val "multmp" builder
      | '<' ->
          let i =
            Llvm.build_fcmp Llvm.Fcmp.Ult lhs_val rhs_val "cmptmp" builder
          in
          (* Convert bool 0/1 to double 0.0 or 1.0 *)
          Llvm.build_uitofp i double_type "booltmp" builder
      | _ ->
          (* If it wasn't a builtin binary operator, it must be a user defined
           * one. Emit a call to it. *)
          let callee = "binary" ^ String.make 1 op in
          let callee =
            match Llvm.lookup_function callee the_module with
            | Some callee -> callee
            | None -> raise_s [%message "unrecognized binop" (op : char)]
          in
          Llvm.build_call callee [|lhs_val; rhs_val|] "binop" builder )
  | Ast.Expr.Call (callee_name, args) ->
      (* Look up the name in the module table. *)
      let callee =
        match Llvm.lookup_function callee_name the_module with
        | Some callee -> callee
        | None ->
            raise_s [%message "undefined function" (callee_name : string)]
      in
      (* If argument mismatch error. *)
      if Int.( = ) (Array.length (Llvm.params callee)) (List.length args) then
        ()
      else
        raise_s
          [%message "incorrect number of arguments" (callee_name : string)] ;
      let args = Array.map (Array.of_list args) ~f:codegen_expr in
      Llvm.build_call callee args "calltmp" builder
  | Ast.Expr.If (condition, then_, else_) ->
      let cond = codegen_expr condition in
      (* Convert condition to a bool by comparing equal to 0.0 *)
      let zero = Llvm.const_float double_type 0.0 in
      let cond_val =
        Llvm.build_fcmp Llvm.Fcmp.One cond zero "ifcond" builder
      in
      (* Grab the first block so that we might later add the conditional branch
       * to it at the end of the function. *)
      let start_bb = Llvm.insertion_block builder in
      let the_function = Llvm.block_parent start_bb in
      let then_bb = Llvm.append_block context "then" the_function in
      (* Emit 'then' value. *)
      Llvm.position_at_end then_bb builder ;
      let then_val = codegen_expr then_ in
      (* Codegen of 'then' can change the current block, update then_bb for the
       * phi. We create a new name because one is used for the phi node, and the
       * other is used for the conditional branch. *)
      let new_then_bb = Llvm.insertion_block builder in
      (* Emit 'else' value. *)
      let else_bb = Llvm.append_block context "else" the_function in
      Llvm.position_at_end else_bb builder ;
      let else_val = codegen_expr else_ in
      (* Codegen of 'else' can change the current block, update else_bb for the
       * phi. *)
      let new_else_bb = Llvm.insertion_block builder in
      (* Emit merge block. *)
      let merge_bb = Llvm.append_block context "ifcont" the_function in
      Llvm.position_at_end merge_bb builder ;
      let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
      let phi = Llvm.build_phi incoming "iftmp" builder in
      (* Return to the start block to add the conditional branch. *)
      Llvm.position_at_end start_bb builder ;
      Llvm.build_cond_br cond_val then_bb else_bb builder |> ignore ;
      (* Set a unconditional branch at the end of the 'then' block and the
       * 'else' block to the 'merge' block. *)
      Llvm.position_at_end new_then_bb builder ;
      Llvm.build_br merge_bb builder |> ignore ;
      Llvm.position_at_end new_else_bb builder ;
      Llvm.build_br merge_bb builder |> ignore ;
      (* Finally, set the builder to the end of the merge block. *)
      Llvm.position_at_end merge_bb builder ;
      phi
  | Ast.Expr.For (var_name, start, end_, step, body) ->
      (* Output this as:
       *   var = alloca double
       *   ...
       *   start = startexpr
       *   store start -> var
       *   goto loop
       * loop:
       *   ...
       *   bodyexpr
       *   ...
       * loopend:
       *   step = stepexpr
       *   endcond = endexpr
       *
       *   curvar = load var
       *   nextvar = curvar + step
       *   store nextvar -> var
       *   br endcond, loop, endloop
       * outloop: *)
      let the_function = Llvm.block_parent (Llvm.insertion_block builder) in
      (* Create an alloca for the variable in the entry block. *)
      let alloca = create_entry_block_alloca the_function var_name in
      (* Emit the start code first, without 'variable' in scope. *)
      let start_val = codegen_expr start in
      (* Store the value into the alloca. *)
      Llvm.build_store start_val alloca builder |> ignore ;
      (* Make the new basic block for the loop header, inserting after current
       * block. *)
      let loop_bb = Llvm.append_block context "loop" the_function in
      (* Insert an explicit fall through from the current block to the
       * loop_bb. *)
      Llvm.build_br loop_bb builder |> ignore ;
      (* Start insertion in loop_bb. *)
      Llvm.position_at_end loop_bb builder ;
      (* Within the loop, the variable is defined equal to the PHI node. If it
       * shadows an existing variable, we have to restore it, so save it
       * now. *)
      let old_val = Hashtbl.find named_values var_name in
      Hashtbl.set named_values ~key:var_name ~data:alloca ;
      (* Emit the body of the loop.  This, like any other expr, can change the
       * current BB.  Note that we ignore the value computed by the body, but
       * don't allow an error *)
      codegen_expr body |> ignore ;
      (* Emit the step value. *)
      let step_val =
        match step with
        | Some step -> codegen_expr step
        (* If not specified, use 1.0. *)
        | None -> Llvm.const_float double_type 1.0
      in
      (* Compute the end condition. *)
      let end_cond = codegen_expr end_ in
      (* Reload, increment, and restore the alloca. This handles the case where
       * the body of the loop mutates the variable. *)
      let cur_var = Llvm.build_load alloca var_name builder in
      let next_var = Llvm.build_fadd cur_var step_val "nextvar" builder in
      Llvm.build_store next_var alloca builder |> ignore ;
      (* Convert condition to a bool by comparing equal to 0.0. *)
      let zero = Llvm.const_float double_type 0.0 in
      let end_cond =
        Llvm.build_fcmp Llvm.Fcmp.One end_cond zero "loopcond" builder
      in
      (* Create the "after loop" block and insert it. *)
      let after_bb = Llvm.append_block context "afterloop" the_function in
      (* Insert the conditional branch into the end of loop_end_bb. *)
      Llvm.build_cond_br end_cond loop_bb after_bb builder |> ignore ;
      (* Any new code will be inserted in after_bb. *)
      Llvm.position_at_end after_bb builder ;
      (* Restore the unshadowed variable. *)
      ( match old_val with
      | Some old_val -> Hashtbl.set named_values ~key:var_name ~data:old_val
      | None -> () ) ;
      (* for expr always returns 0.0. *)
      Llvm.const_null double_type
  | Ast.Expr.Unary (op, operand) ->
      let operand = codegen_expr operand in
      let callee = "unary" ^ String.make 1 op in
      let callee =
        match Llvm.lookup_function callee the_module with
        | Some callee -> callee
        | None -> raise_s [%message "unknown unary operator" (op : char)]
      in
      Llvm.build_call callee [|operand|] "unop" builder

let codegen_proto_existing = function
  | Ast.Prototype (name, args) | Ast.BinOpPrototype (name, args, _) ->
      (* Make the function type: double(double,double) etc. *)
      Hashtbl.clear named_values ;
      let doubles = Array.create ~len:(List.length args) double_type in
      let ft = Llvm.function_type double_type doubles in
      let f, existing =
        match Llvm.lookup_function name the_module with
        | None -> (Llvm.declare_function name ft the_module, `Existing)
        (* If 'f' conflicted, there was already something named 'name'. If it
         * has a body, don't allow redefinition or reextern. *)
        | Some f ->
            (* If 'f' already has a body, reject this. *)
            if Int.(Array.length (Llvm.basic_blocks f) = 0) then ()
            else raise_s [%message "redefinition of function" (name : string)] ;
            (* If 'f' took a different number of arguments, reject. *)
            if Int.(Array.length (Llvm.params f) = List.length args) then ()
            else
              raise_s
                [%message
                  "redefinition of function with a different number of args"
                    (name : string)] ;
            (f, `Not_existing)
      in
      (* Set names for all arguments. *)
      Array.iteri (Llvm.params f) ~f:(fun i a ->
          let name = List.nth_exn args i in
          Llvm.set_value_name name a ;
          Hashtbl.add_exn named_values ~key:name ~data:a ) ;
      (f, existing)

(* Create an alloca for each argument and register the argument in the symbol
 * table so that references to it will succeed. *)
let create_argument_allocas the_function proto =
  let args =
    match proto with
    | Ast.Prototype (_, args) | Ast.BinOpPrototype (_, args, _) -> args
  in
  Array.iteri (Llvm.params the_function) ~f:(fun i ai ->
      let var_name = List.nth_exn args i in
      (* Create an alloca for this variable. *)
      let alloca = create_entry_block_alloca the_function var_name in
      (* Store the initial value into the alloca. *)
      Llvm.build_store ai alloca builder |> ignore ;
      (* Add arguments to variable symbol table. *)
      Hashtbl.set named_values ~key:var_name ~data:alloca )

let codegen_func the_fpm = function
  | Ast.Function (proto, body) -> (
      let the_function, existing = codegen_proto_existing proto in
      (* If this is an operator, install it. *)
      ( match proto with
      | Ast.BinOpPrototype (name, _args, prec) ->
          let op = name.[String.length name - 1] in
          Hashtbl.add_exn Ast.binop_precedence ~key:op ~data:prec
      | _ -> () ) ;
      (* Create a new basic block to start insertion into. *)
      let bb = Llvm.append_block context "entry" the_function in
      Llvm.position_at_end bb builder ;
      try
        (* Add all arguments to the symbol table and create their allocas. *)
        create_argument_allocas the_function proto ;
        let return_val = codegen_expr body in
        (* Finish off the function. *)
        let _ : Llvm.llvalue = Llvm.build_ret return_val builder in
        (* Validate the generated code, checking for consistency. *)
        ( match Llvm_analysis.verify_function the_function with
        | true -> ()
        | false ->
            printf "invalid function generated\n%s\n"
              (Llvm.string_of_llvalue the_function) ;
            Llvm_analysis.assert_valid_function the_function ) ;
        (* Optimize the function. *)
        let _ : bool = Llvm.PassManager.run_function the_function the_fpm in
        the_function
      with e ->
        ( match existing with
        | `Not_existing -> Llvm.delete_function the_function
        | `Existing ->
            Array.iter (Llvm.basic_blocks the_function) ~f:Llvm.delete_block ) ;
        raise e )

let codegen_proto proto = codegen_proto_existing proto |> fst
