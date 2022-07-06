module Term (X : Suplex.Lang.S) = struct
  let leaf = X.int64 0

  let node x y = X.tuple [X.T.Ex_repr x; X.T.Ex_repr y]

  let term =
    let open X in
    int64 3 >|= fun i ->
    int64 4 >|= fun j ->
    add Int64 i j >|= fun isum64 ->
    int16 2 >|= fun i ->
    int16 4 >|= fun j ->
    add Int16 i j >|= fun isum16 ->
    (* print isum64 >|= fun _ ->
     * print isum16 >|= fun _ ->
     * print (float 888.0) >|= fun _ -> *)
    node isum16 isum64 >|= fun leaf ->
    node leaf leaf >|= fun node ->
    proj leaf { index = 0; desired_type = T.(TNum Int16) } >|= fun _result ->
    proj
      node
      { index = 0;
        desired_type = T.(TTpl [Ex_typ (TNum Int16); Ex_typ (TNum Int64)])
      }
    >|= fun result1 ->
    proj result1 { index = 1; desired_type = T.(TNum Int64) }
    >|= fun _result2 -> X.unit
  (* print _result2 *)
end

(* module Foo = Suplex.Compile.Codegen (Term) *)
(* module S = Suplex.S.Repr *)

(* let _ = Suplex.S.res *)

open Suplex.S2

module Repr = LLVM_repr ()

type record = { f1 : int64; f2 : float }

type record' = record

type with_array = { next : with_array ref }

module Record = struct
  open Repr
  open Type_system

  let record = empty_rec |+ field "f1" int64 |+ field "f2" F64.t

  let t = seal record

  let make i f =
    struct_
      record
      (fun vec ->
        match vec with Cons_vec (f2, Cons_vec (f1, Nil_vec)) -> { f1; f2 })
      f
      i

  let zero = make (I64.v 0L) (F64.v 0.0)

  let (((), f1), f2) =
    projs record (fun { f1; f2 } -> Cons_vec (f2, Cons_vec (f1, Nil_vec)))
end

let (state, fundecl) =
  Repr.LLVM_state.run
  @@
  let open Repr.LLVM_state in
  let* print_int64 =
    Repr.register_external
      ~name:"print_int64"
      ~signature:
        Repr.Prototype.(
          Repr.Type_system.int64 @-> returning Repr.Type_system.unit)
  in

  let* edit_array_fundecl =
    let open Repr in
    let open Type_system in
    fundecl
      ~name:"edit_array"
      ~signature:Prototype.(returning I64.t)
      ~local:
        Stack_frame.(
          array_i64
            Record.t
            (Record.make (I64.v 1L) (F64.v 2.))
            (I64.v 2L :> (_, unknown) m)
          @+ empty)
      ~body:(fun arr () ->
        let* elt1 = get arr (I64.v 0L) in
        let* elt2 = get arr (I64.v 1L) in
        let* v1 = load @@ Record.f1.proj elt1 in
        let* v2 = load @@ Record.f1.proj elt2 in
        let* _ = call print_int64 Prototype.[v1] in
        let* _ = call print_int64 Prototype.[v2] in

        let* _ = set arr (I64.v 1L) (load elt1) in
        let* _ = set arr (I64.v 0L) (load elt2) in
        let* elt1 = get arr (I64.v 0L) in
        let* elt2 = get arr (I64.v 1L) in
        let* v1 = load @@ Record.f1.proj elt1 in
        let* v2 = load @@ Record.f1.proj elt2 in
        let* _ = call print_int64 Prototype.[v1] in
        let* _ = call print_int64 Prototype.[v2] in

        I64.add (load @@ Record.f1.proj elt1) (load @@ Record.f1.proj elt2))
  in

  let* fact =
    let open Repr in
    fundecl
      ~name:"fact"
      ~signature:Prototype.(Type_system.int64 @-> returning Type_system.int64)
      ~local:Stack_frame.(single Type_system.int64 (I64.v 1L) @+ empty)
      ~body:(fun acc (* _strct *) (n, ()) ->
        let* n = cond (I64.eq n n) (fun _ -> n) in
        let* n =
          switch_i64
            n
            ~cases:
              [| (0L, fun () -> I64.add n (I64.v 0L));
                 (3L, fun () -> I64.add n (I64.v 3L))
              |]
            ~default:(fun () -> n)
        in
        let* _ =
          for_
            ~init:(I64.v 1L)
            ~pred:(fun i -> I64.le i n)
            ~step:(fun i -> I64.add i (I64.v 1L))
            (fun i -> store acc (I64.mul (load acc) i))
        in
        (load acc :> (_, unknown) Repr.Type_system.m))
  in

  let* init_array_then_sum =
    let open Repr in
    let open Type_system in
    fundecl
      ~name:"init_then_sum"
      ~signature:Prototype.(returning I64.t)
      ~local:
        Stack_frame.(
          array_i64 int64 (I64.v 1L) (I64.v 10L :> (int64, unknown) m)
          @+ single int64 (I64.v 0L)
          @+ single Record.t (Record.make (I64.v 13L) (F64.v 12.))
          @+ empty)
      ~body:(fun arr acc strct () ->
        let* _ =
          for_
            ~init:(I64.v 0L)
            ~pred:(fun i -> I64.lt i (I64.v 10L))
            ~step:(fun i -> I64.add i (I64.v 1L))
            (fun i -> store acc (I64.add (load (get arr i)) (load acc)))
        in
        let* _ = store (Record.f1.proj strct) (I64.v 66L) in
        let* x1 = load (Record.f1.proj strct) in
        I64.add (load acc) x1)
  in

  let* main =
    let open Repr in
    fundecl
      ~name:"main"
      ~signature:Prototype.(returning Type_system.int64)
      ~local:Stack_frame.empty
      ~body:(fun () ->
        let* fact_res = call fact Prototype.[I64.v 6L] in
        let* sum_res = call init_array_then_sum Prototype.[] in
        let* _zero = call edit_array_fundecl Prototype.[] in
        I64.add fact_res sum_res)
  in
  return main

let res =
  let engine = Llvm_executionengine.create state.llvm_module in

  let fpm = Llvm.PassManager.create () in
  Llvm.dump_module state.llvm_module ;
  let _ = Llvm.PassManager.run_module state.llvm_module fpm in

  let fn_typ : (unit -> int64) Ctypes.fn =
    Ctypes.(void @-> returning int64_t)
  in
  let fn_ptr_typ = Foreign.funptr fn_typ in
  let f = Llvm_executionengine.get_function_address "main" fn_ptr_typ engine in
  let res = f () in
  let () = Format.printf "executing = %Ld@." res in
  Llvm_executionengine.dispose engine
