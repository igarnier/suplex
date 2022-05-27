module Codegen (Term : functor (X : Lang.S) -> sig
  val term : X.T.unit_t X.repr
end) =
struct
  let main =
    let open LlvmRepr.State in
    lmodule >>= fun llmodule ->
    context >>= fun ctxt ->
    builder >>= fun bld ->
    LlvmRepr.type_to_llvm LlvmRepr.T.(TNum Int8) >>= fun main_rettyp ->
    let ft = Llvm.function_type main_rettyp [||] in
    let fv = Llvm.declare_function "main" ft llmodule in
    let bb = Llvm.append_block ctxt "entry" fv in
    Llvm.position_at_end bb bld ;
    let module Body = Term (LlvmRepr) in
    Body.term >>= fun ret_val ->
    let _ = Llvm.build_ret ret_val.llval bld in
    return fv

  let process_main =
    let open LlvmRepr.State in
    lmodule >>= fun llmodule ->
    main >>= fun main_v ->
    let fpm = Llvm.PassManager.create_function llmodule in
    let _ = Llvm.PassManager.initialize fpm in
    Llvm.dump_module llmodule ;
    Llvm_analysis.assert_valid_function main_v ;
    let _ = Llvm.PassManager.run_function main_v fpm in
    return ()

  let run_code =
    assert (Llvm_executionengine.initialize ()) ;
    let state = LlvmRepr.fresh_state () in
    let { LlvmRepr.State.state; _ } = process_main state.state in
    let engine = Llvm_executionengine.create state.llvm_module in
    let fn_typ : (unit -> char) Ctypes.fn = Ctypes.(void @-> returning char) in
    let fn_ptr_typ = Foreign.funptr fn_typ in
    let f =
      Llvm_executionengine.get_function_address "main" fn_ptr_typ engine
    in
    let () = Format.printf "executing@." in
    ignore (f ()) ;
    Llvm_executionengine.dispose engine
end
