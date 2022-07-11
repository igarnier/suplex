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
        match vec with
        | Cons_vec ((f2 : float), Cons_vec ((f1 : int64), Nil_vec)) ->
            { f1; f2 })
      f
      i

  let zero = make (I64.v 0L) (F64.v 0.0)

  let (((), f1), f2) =
    projs record (fun { f1; f2 } -> Cons_vec (f2, Cons_vec (f1, Nil_vec)))
end

(* let (state, _fundecl) =
 *   Repr.LLVM_state.run
 *   @@
 *   let open Repr.LLVM_state in
 *   let* _print_int64 =
 *     Repr.register_external
 *       ~name:"print_int64"
 *       ~signature:
 *         Repr.Prototype.(
 *           Repr.Type_system.int64 @-> returning Repr.Type_system.unit)
 *   in
 *
 *   let* edit_array_fundecl =
 *     let open Repr in
 *     let open Type_system in
 *     fundecl
 *       ~name:"edit_array"
 *       ~signature:Prototype.(returning I64.t)
 *       ~local:
 *         Stack_frame.(
 *           arr
 *             Record.t
 *             (\* (Record.make (I64.v 1L) (F64.v 2.)) *\)
 *             (I64.v 2L :> (_, unknown) m)
 *           @+ empty)
 *       ~body:(fun arr () ->
 *         let* elt1 = get arr (I64.v 0L) in
 *         let* elt2 = get arr (I64.v 1L) in
 *
 *         (\* let* v1 = load @@ Record.f1.get elt1 in
 *          * let* v2 = load @@ Record.f1.get elt2 in *\)
 *
 *         (\* let* _ = call print_int64 Prototype.[v1] in
 *          * let* _ = call print_int64 Prototype.[v2] in *\)
 *         let* _ = set arr (I64.v 1L) elt1 in
 *         let* _ = set arr (I64.v 0L) elt2 in
 *         let* elt1 = get arr (I64.v 0L) in
 *         let* elt2 = get arr (I64.v 1L) in
 *
 *         (\* let* v1 = load @@ Record.f1.get elt1 in
 *          * let* v2 = load @@ Record.f1.get elt2 in *\)
 *
 *         (\* let* _ = call print_int64 Prototype.[v1] in
 *          * let* _ = call print_int64 Prototype.[v2] in *\)
 *         I64.add (Record.f1.get elt1) (Record.f1.get elt2))
 *   in
 *
 *   let* fact =
 *     let open Repr in
 *     fundecl
 *       ~name:"fact"
 *       ~signature:Prototype.(Type_system.int64 @-> returning Type_system.int64)
 *       ~local:Stack_frame.(num Type_system.Int64_num @+ empty)
 *       ~body:(fun acc (n, ()) ->
 *         let* _ = store acc I64.one in
 *         let* n = cond (I64.eq n n) (fun _ -> n) in
 *         let* n =
 *           switch_i64
 *             n
 *             ~cases:
 *               [| (0L, fun () -> I64.add n (I64.v 0L));
 *                  (3L, fun () -> I64.add n (I64.v 3L))
 *               |]
 *             ~default:(fun () -> n)
 *         in
 *         let* _ =
 *           for_
 *             ~init:(I64.v 1L)
 *             ~pred:(fun i -> I64.le i n)
 *             ~step:(fun i -> I64.add i (I64.v 1L))
 *             (fun i -> store acc (I64.mul (load acc) i))
 *         in
 *         (load acc :> (_, unknown) Repr.Type_system.m))
 *   in
 *
 *   let init_array_macro a length =
 *     let open Repr in
 *     for_
 *       ~init:(I64.v 0L)
 *       ~pred:(fun i -> I64.lt i length)
 *       ~step:(fun i -> I64.add i (I64.v 1L))
 *       (fun i -> set a i I64.zero)
 *   in
 *
 *   let* init_array_then_sum =
 *     let open Repr in
 *     let open Type_system in
 *     fundecl
 *       ~name:"init_then_sum"
 *       ~signature:Prototype.(returning I64.t)
 *       ~local:
 *         Stack_frame.(
 *           arr int64 (\* (I64.v 1L) *\) (I64.v 10L :> (int64, unknown) m)
 *           @+ num Type_system.Int64_num (\* (I64.v 0L) *\)
 *           @+ strct Record.record (\* (Record.make (I64.v 13L) (F64.v 12.)) *\)
 *           @+ empty)
 *       ~body:(fun arr acc strct () ->
 *         let* _ = init_array_macro arr (I64.v 10L) in
 *         let* _ = Record.f1.set strct (I64.v 13L) in
 *         let* _ = Record.f2.set strct (F64.v 12.) in
 *         let* _ =
 *           for_
 *             ~init:(I64.v 0L)
 *             ~pred:(fun i -> I64.lt i (I64.v 10L))
 *             ~step:(fun i -> I64.add i (I64.v 1L))
 *             (fun i -> store acc (I64.add (get arr i) (load acc)))
 *         in
 *         let* _ = Record.f1.set strct (I64.v 66L) in
 *         let* x1 = Record.f1.get strct in
 *         I64.add (load acc) x1)
 *   in
 *
 *   let* main =
 *     let open Repr in
 *     fundecl
 *       ~name:"main"
 *       ~signature:Prototype.(returning Type_system.int64)
 *       ~local:Stack_frame.empty
 *       ~body:(fun () ->
 *         let* fact_res = call fact Prototype.[I64.v 6L] in
 *         let* sum_res = call init_array_then_sum Prototype.[] in
 *         let* _zero = call edit_array_fundecl Prototype.[] in
 *         I64.add fact_res sum_res)
 *   in
 *   return main *)

let run_llvm_program_generic ?(verbose = false) fn_typ main =
  let (state, fundecl) =
    try Repr.LLVM_state.run main
    with Repr.Invalid_llvm_function (m, f) ->
      Llvm.dump_module m ;
      Llvm_analysis.assert_valid_function f ;
      assert false
  in
  let engine = Llvm_executionengine.create state.llvm_module in
  let fpm = Llvm.PassManager.create () in
  if verbose then Llvm.dump_module state.llvm_module ;
  let _ = Llvm.PassManager.run_module state.llvm_module fpm in
  let fn_ptr_typ = Foreign.funptr fn_typ in
  let f =
    Llvm_executionengine.get_function_address
      (Repr.fundecl_name fundecl)
      fn_ptr_typ
      engine
  in
  (engine, f)

let run_llvm_program1 (type a b) ?verbose (fn_typ : (a -> b) Ctypes.fn)
    (main :
      ( (a, _) Repr.Type_system.m * unit,
        (b, unknown) Repr.Type_system.m )
      Repr.fundecl
      Repr.k) inputs : b list =
  let (engine, f) = run_llvm_program_generic ?verbose fn_typ main in
  let res = List.map f inputs in
  Llvm_executionengine.dispose engine ;
  res

let run_llvm_program1_unsafe ?verbose fn_typ main inputs =
  let (engine, f) = run_llvm_program_generic ?verbose fn_typ main in
  let res = List.map f inputs in
  Llvm_executionengine.dispose engine ;
  res

let run_llvm_program2 (type a b c) ?verbose (fn_typ : (a -> b -> c) Ctypes.fn)
    (main :
      ( (a, _) Repr.Type_system.m * ((b, _) Repr.Type_system.m * unit),
        (c, unknown) Repr.Type_system.m )
      Repr.fundecl
      Repr.k) inputs : c list =
  let (engine, f) = run_llvm_program_generic ?verbose fn_typ main in
  let res = List.map (fun (x, y) -> f x y) inputs in
  Llvm_executionengine.dispose engine ;
  res

let test_fact () =
  let rec fact_oracle n =
    if n = 0L then 1L else Int64.mul n (fact_oracle (Int64.pred n))
  in
  Alcotest.(check (list int64))
    "iterative_factorial"
    (List.map fact_oracle [1L; 2L; 3L; 4L; 5L])
  @@ run_llvm_program1
       Ctypes.(int64_t @-> returning int64_t)
       (let open Repr in
       fundecl
         ~name:"iterative_fact"
         ~signature:
           Prototype.(Type_system.int64 @-> returning Type_system.int64)
         ~local:Stack_frame.(num Type_system.Int64_num @+ empty)
         ~body:(fun acc (n, ()) ->
           let* _ = store acc I64.one in
           let* n = cond (I64.eq n n) (fun _ -> n) in
           let* _ =
             for_
               ~init:(I64.v 1L)
               ~pred:(fun i -> I64.le i n)
               ~step:(fun i -> I64.add i (I64.v 1L))
               (fun i -> store acc (I64.mul (load acc) i))
           in
           (load acc :> (_, unknown) Repr.Type_system.m)))
       [1L; 2L; 3L; 4L; 5L]

let test_nested_switch () =
  Alcotest.(check (list int64)) "nested_switch" [0L; 0L; 42L; 1789L; -1L]
  @@ run_llvm_program1
       Ctypes.(int64_t @-> returning int64_t)
       (let open Repr in
       fundecl
         ~name:"nested_switch"
         ~signature:
           Prototype.(Type_system.int64 @-> returning Type_system.int64)
         ~local:Stack_frame.(num Type_system.Int64_num @+ empty)
         ~body:(fun local (x, ()) ->
           let* _ = store local (I64.div x (I64.v 2L)) in
           switch_i64
             (load local)
             ~cases:
               [| (0L, fun () -> (I64.zero :> (_, unknown) Type_system.m));
                  ( 5L,
                    fun () ->
                      switch_i64
                        x
                        ~cases:
                          [| ( 11L,
                               fun () ->
                                 (I64.v 42L :> (_, unknown) Type_system.m) )
                          |]
                        ~default:(fun () ->
                          (I64.v 1789L :> (_, unknown) Type_system.m)) )
               |]
             ~default:(fun () -> (I64.v (-1L) :> (_, unknown) Type_system.m))))
       [0L; 1L; 11L; 10L; 12L]

let test_nested_cond () =
  Alcotest.(check (list int64)) "nested_cond" [0L; 0L; 42L; 1789L; 42L; 1789L]
  @@ run_llvm_program1
       Ctypes.(int64_t @-> returning int64_t)
       (let open Repr in
       fundecl
         ~name:"nested_switch"
         ~signature:
           Prototype.(Type_system.int64 @-> returning Type_system.int64)
         ~local:Stack_frame.(num Type_system.Int64_num @+ empty)
         ~body:(fun local (x, ()) ->
           let* _ = store local (I64.div x (I64.v 2L)) in
           let* v = load local in
           cond (I64.eq v I64.zero) (function
               | true -> (I64.zero :> (_, unknown) Type_system.m)
               | false ->
                   cond
                     (I64.eq v (I64.v 5L))
                     (function
                       | true ->
                           cond
                             (I64.eq x (I64.v 11L))
                             (function
                               | true ->
                                   (I64.v 42L :> (_, unknown) Type_system.m)
                               | false ->
                                   (I64.v 1789L :> (_, unknown) Type_system.m))
                       | false ->
                           cond
                             (I64.eq x (I64.v 12L))
                             (function
                               | true ->
                                   (I64.v 42L :> (_, unknown) Type_system.m)
                               | false ->
                                   (I64.v 1789L :> (_, unknown) Type_system.m))))))
       [0L; 1L; 11L; 10L; 12L; 13L]

type int64_pair = { x : int64; y : int64 }

(* A struct containing a pair of int64. *)
let (ctypes_int64_pair, x_field, y_field) =
  let open Ctypes in
  let p : int64_pair structure typ = structure "int64_pair" in
  let x = field p "x" int64_t in
  let y = field p "y" int64_t in
  seal p ;
  (p, x, y)

let make_ctypes_int64_pair x y =
  let v = Ctypes.make ctypes_int64_pair in
  Ctypes.setf v x_field x ;
  Ctypes.setf v y_field y ;
  Ctypes.allocate ctypes_int64_pair v

module Int64_pair = struct
  open Repr
  open Type_system

  let record = empty_rec |+ field "x" int64 |+ field "y" int64

  let t = seal record

  let make x y =
    struct_
      record
      (fun vec ->
        match vec with Cons_vec (y, Cons_vec (x, Nil_vec)) -> { x; y })
      y
      x

  let zero = make (I64.v 0L) (I64.v 0L)

  let (((), f1), f2) =
    projs record (fun { x; y } -> Cons_vec (y, Cons_vec (x, Nil_vec)))
end

let test_struct_alloca () =
  Alcotest.(check (list int64)) "struct_alloca" [2L; 3L; 4L; 5L; 6L]
  @@ run_llvm_program2
       Ctypes.(int64_t @-> int64_t @-> returning int64_t)
       (let open Repr in
       fundecl
         ~name:"struct_alloca"
         ~signature:
           Prototype.(
             Type_system.int64 @-> Type_system.int64
             @-> returning Type_system.int64)
         ~local:Stack_frame.(strct Int64_pair.record @+ empty)
         ~body:(fun acc (x, (y, ())) ->
           let* _ = Int64_pair.f1.set acc x in
           let* _ = Int64_pair.f2.set acc y in
           I64.add (Int64_pair.f1.get acc) (Int64_pair.f2.get acc)))
       [(1L, 1L); (2L, 1L); (3L, 1L); (4L, 1L); (5L, 1L)]

let test_struct_arg () =
  Alcotest.(check (list int64)) "struct_arg" [2L; 3L; 4L; 5L; 6L]
  @@ run_llvm_program1_unsafe
       Ctypes.(ptr ctypes_int64_pair @-> returning int64_t)
       (let open Repr in
       fundecl
         ~name:"struct_arg"
         ~signature:Prototype.(Int64_pair.t @-> returning Type_system.int64)
         ~local:Stack_frame.empty
         ~body:(fun (x, ()) ->
           I64.add (Int64_pair.f1.get x) (Int64_pair.f2.get x)))
       (List.map
          (fun (x, y) -> make_ctypes_int64_pair x y)
          [(1L, 1L); (2L, 1L); (3L, 1L); (4L, 1L); (5L, 1L)])

let test_struct_const_init () =
  Alcotest.(check (list int64)) "struct_const_init" [85L; 85L; 85L; 85L; 85L]
  @@ run_llvm_program1_unsafe
       Ctypes.(ptr ctypes_int64_pair @-> returning int64_t)
       (let open Repr in
       fundecl
         ~name:"struct_const_init"
         ~signature:Prototype.(Int64_pair.t @-> returning Type_system.int64)
         ~local:Stack_frame.empty
         ~body:(fun (x, ()) ->
           let* _ = Int64_pair.f1.set x (I64.v 42L) in
           let* _ = Int64_pair.f2.set x (I64.v 43L) in
           (* let* _ = store x (Int64_pair.make (I64.v 42L) (I64.v 43L)) in *)
           I64.add (Int64_pair.f1.get x) (Int64_pair.f2.get x)))
       (List.map
          (fun (x, y) -> make_ctypes_int64_pair x y)
          [(1L, 1L); (2L, 1L); (3L, 1L); (4L, 1L); (5L, 1L)])

let array_to_ctypes (a : int64 array) =
  let open Ctypes in
  let arr = allocate_n int64_t ~count:(Array.length a) in
  let rec loop ptr n =
    if n = Array.length a then ()
    else
      let () = ptr <-@ a.(n) in
      loop (ptr +@ 1) (n + 1)
  in
  loop arr 0 ;
  arr

let test_array_arg () =
  Alcotest.(check (list int64)) "array_arg" [5L; 10L]
  @@ run_llvm_program1_unsafe
       Ctypes.(ptr int64_t @-> returning int64_t)
       (let open Repr in
       fundecl
         ~name:"array_arg"
         ~signature:
           Prototype.(Type_system.(vec int64) @-> returning Type_system.int64)
         ~local:Stack_frame.(num Type_system.Int64_num @+ empty)
         ~body:(fun acc (x, ()) ->
           let* _ = store acc I64.zero in
           let* _ =
             for_
               ~init:(I64.v 0L)
               ~pred:(fun i -> I64.le i (I64.v 4L))
               ~step:(fun i -> I64.add i (I64.v 1L))
               (fun i -> store acc (I64.add (load acc) (get x i)))
           in
           (load acc :> (_, unknown) Type_system.m)))
       [ array_to_ctypes [| 1L; 1L; 1L; 1L; 1L |];
         array_to_ctypes (Array.init 5 Int64.of_int) ]

let test_alloca_struct_array () =
  Alcotest.(check (list bool)) "alloca_struct_array" [true]
  @@ run_llvm_program1_unsafe
       Ctypes.(void @-> returning bool)
       (let open Repr in
       fundecl
         ~name:"alloca_struct_array"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.bool)
         ~local:
           Stack_frame.(
             arr Int64_pair.t (I64.v 2L :> (_, unknown) Type_system.m)
             @+ strct Int64_pair.record @+ empty)
         ~body:(fun arr tmp (_, ()) ->
           let* s1 = get arr (I64.v 0L) in
           let* s2 = get arr (I64.v 1L) in
           let* _ = Int64_pair.f1.set s1 (I64.v 1L) in
           let* _ = Int64_pair.f2.set s1 (I64.v 2L) in
           let* _ = Int64_pair.f1.set s2 (I64.v 3L) in
           let* _ = Int64_pair.f2.set s2 (I64.v 4L) in

           let* _ = Int64_pair.f1.set tmp (Int64_pair.f1.get s1) in
           let* _ = Int64_pair.f2.set tmp (Int64_pair.f2.get s1) in

           let* _ = set arr (I64.v 0L) s2 in
           let* _ = set arr (I64.v 1L) tmp in

           let* w = Int64_pair.f1.get s1 in
           let* x = Int64_pair.f2.get s1 in
           let* y = Int64_pair.f1.get s2 in
           let* z = Int64_pair.f2.get s2 in

           I64.(
             eq w (I64.v 3L)
             && eq x (I64.v 4L)
             && eq y (I64.v 1L)
             && eq z (I64.v 2L))))
       [()]

let wrong_array_get () =
  Alcotest.(check (list unit)) "struct_arg" [()]
  @@ run_llvm_program1_unsafe
       Ctypes.(void @-> returning void)
       (let open Repr in
       fundecl
         ~name:"struct_arg"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.unit)
         ~local:Stack_frame.empty
         ~body:(fun (_x, ()) ->
           let* a = array [| unit; unit; unit |] in
           (get a I64.zero :> (_, unknown) Type_system.m)))
       [()]

let () =
  let open Alcotest in
  run
    "llvm-codegen"
    [ ( "basic",
        [ test_case "fact" `Quick test_fact;
          test_case "fact" `Quick test_fact;
          test_case "nested_switch" `Quick test_nested_switch;
          test_case "nested_cond" `Quick test_nested_cond;
          test_case "struct_alloca" `Quick test_struct_alloca;
          test_case "struct_arg" `Quick test_struct_arg;
          test_case "struct_const_init" `Quick test_struct_const_init;
          test_case "array_arg" `Quick test_array_arg;
          test_case "alloca_struct_array" `Quick test_alloca_struct_array
          (* test_case "wrong_array_get" `Quick wrong_array_get *) ] ) ]
