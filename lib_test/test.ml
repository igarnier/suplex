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

  let (((), f1), f2) =
    projs record (fun { f1; f2 } -> Cons_vec (f2, Cons_vec (f1, Nil_vec)))
end

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
      (a Repr.Type_system.m * unit, b Repr.Type_system.m) Repr.fundecl Repr.k)
    inputs : b list =
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
      ( a Repr.Type_system.m * (b Repr.Type_system.m * unit),
        c Repr.Type_system.m )
      Repr.fundecl
      Repr.k) inputs : c list =
  let (engine, f) = run_llvm_program_generic ?verbose fn_typ main in
  let res = List.map (fun (x, y) -> f x y) inputs in
  Llvm_executionengine.dispose engine ;
  res

let test_store_unit () =
  Alcotest.(check (list unit)) "store_unit" [()]
  @@ run_llvm_program1_unsafe
       Ctypes.(void @-> returning void)
       (let open Repr in
       fundecl
         ~name:"store_unit"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.unit)
         ~local:Stack_frame.(unit @+ empty)
         ~body:(fun _self unit_ptr (_x, ()) ->
           let* _ = store unit_ptr unit in
           load unit_ptr))
       [()]

let test_store_bool () =
  Alcotest.(check (list bool)) "store_bool" [true; false]
  @@ run_llvm_program1_unsafe
       Ctypes.(bool @-> returning bool)
       (let open Repr in
       fundecl
         ~name:"store_bool"
         ~signature:Prototype.(Type_system.bool @-> returning Type_system.bool)
         ~local:Stack_frame.(bool @+ empty)
         ~body:(fun _self bool_ptr (x, ()) ->
           let* _ = store bool_ptr x in
           load bool_ptr))
       [true; false]

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
         ~body:(fun _self acc (n, ()) ->
           let* _ = store acc I64.one in
           let* n = cond (I64.eq n n) (fun _ -> n) in
           let* _ =
             for_
               ~init:(I64.v 1L)
               ~pred:(fun i -> I64.le i n)
               ~step:(fun i -> I64.add i (I64.v 1L))
               (fun i -> store acc (I64.mul (load acc) i))
           in
           load acc))
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
         ~body:(fun _self local (x, ()) ->
           let* _ = store local (I64.div x (I64.v 2L)) in
           switch_i64
             (load local)
             ~cases:
               [| (0L, fun () -> I64.zero);
                  ( 5L,
                    fun () ->
                      switch_i64
                        x
                        ~cases:[| (11L, fun () -> I64.v 42L) |]
                        ~default:(fun () -> I64.v 1789L) )
               |]
             ~default:(fun () -> I64.v (-1L))))
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
         ~body:(fun _self local (x, ()) ->
           let* _ = store local (I64.div x (I64.v 2L)) in
           let* v = load local in
           cond (I64.eq v I64.zero) (function
               | true -> I64.zero
               | false ->
                   cond
                     (I64.eq v (I64.v 5L))
                     (function
                       | true ->
                           cond
                             (I64.eq x (I64.v 11L))
                             (function
                               | true -> I64.v 42L | false -> I64.v 1789L)
                       | false ->
                           cond
                             (I64.eq x (I64.v 12L))
                             (function
                               | true -> I64.v 42L | false -> I64.v 1789L)))))
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

  let r = empty_rec |+ field "x" int64 |+ field "y" int64

  let t = seal r

  let (((), f1), f2) =
    projs r (fun { x; y } -> Cons_vec (y, Cons_vec (x, Nil_vec)))

  let eq s1 s2 = I64.eq s1.%{f1} s2.%{f1} && I64.eq s1.%{f2} s2.%{f2}
end

let ( >> ) = Repr.seq

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
         ~local:Stack_frame.(strct Int64_pair.r @+ empty)
         ~body:(fun _self acc (x, (y, ())) ->
           let* _ = acc.%{Int64_pair.f1} <- x in
           let* _ = acc.%{Int64_pair.f2} <- y in
           I64.add acc.%{Int64_pair.f1} acc.%{Int64_pair.f2}))
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
         ~body:(fun _self (x, ()) ->
           I64.add x.%{Int64_pair.f1} x.%{Int64_pair.f2}))
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
         ~body:(fun _self (x, ()) ->
           let* _ = x.%{Int64_pair.f1} <- I64.v 42L in
           let* _ = x.%{Int64_pair.f2} <- I64.v 43L in
           I64.add x.%{Int64_pair.f1} x.%{Int64_pair.f2}))
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
           Prototype.(
             Type_system.(arr Size_unk int64) @-> returning Type_system.int64)
         ~local:Stack_frame.(num Type_system.Int64_num @+ empty)
         ~body:(fun _self acc (x, ()) ->
           let* _ = store acc I64.zero in
           let* _ =
             for_
               ~init:(I64.v 0L)
               ~pred:(fun i -> I64.le i (I64.v 4L))
               ~step:(fun i -> I64.add i (I64.v 1L))
               (fun i -> store acc (I64.add (load acc) (get x i)))
           in
           load acc))
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
             arr Int64_pair.t (I64.v 2L) @+ strct Int64_pair.r @+ empty)
         ~body:(fun _self arr tmp (_, ()) ->
           let* s1 = get arr (I64.v 0L) in
           let* s2 = get arr (I64.v 1L) in
           let* _ = s1.%{Int64_pair.f1} <- I64.v 1L in
           let* _ = s1.%{Int64_pair.f2} <- I64.v 2L in

           let* _ = s2.%{Int64_pair.f1} <- I64.v 3L in
           let* _ = s2.%{Int64_pair.f2} <- I64.v 4L in

           let* _ = tmp.%{Int64_pair.f1} <- s1.%{Int64_pair.f1} in
           let* _ = tmp.%{Int64_pair.f2} <- s1.%{Int64_pair.f2} in

           let* _ = set arr (I64.v 0L) s2 in
           let* _ = set arr (I64.v 1L) tmp in

           let* w = s1.%{Int64_pair.f1} in
           let* x = s1.%{Int64_pair.f2} in
           let* y = s2.%{Int64_pair.f1} in
           let* z = s2.%{Int64_pair.f2} in

           I64.(
             eq w (I64.v 3L)
             && eq x (I64.v 4L)
             && eq y (I64.v 1L)
             && eq z (I64.v 2L))))
       [()]

let test_alloca_struct_cst_array () =
  Alcotest.(check (list bool)) "alloca_struct_cst_array" [true]
  @@ run_llvm_program1_unsafe
       Ctypes.(void @-> returning bool)
       (let open Repr in
       fundecl
         ~name:"alloca_struct_cst_array"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.bool)
         ~local:
           Stack_frame.(arr_cst Int64_pair.t 2L @+ strct Int64_pair.r @+ empty)
         ~body:(fun _self arr tmp (_, ()) ->
           let* s1 = get arr (I64.v 0L) in
           let* s2 = get arr (I64.v 1L) in
           let* _ = s1.%{Int64_pair.f1} <- I64.v 1L in
           let* _ = s1.%{Int64_pair.f2} <- I64.v 2L in

           let* _ = s2.%{Int64_pair.f1} <- I64.v 3L in
           let* _ = s2.%{Int64_pair.f2} <- I64.v 4L in

           let* _ = tmp.%{Int64_pair.f1} <- s1.%{Int64_pair.f1} in
           let* _ = tmp.%{Int64_pair.f2} <- s1.%{Int64_pair.f2} in

           let* _ = set arr (I64.v 0L) s2 in
           let* _ = set arr (I64.v 1L) tmp in

           let* w = s1.%{Int64_pair.f1} in
           let* x = s1.%{Int64_pair.f2} in
           let* y = s2.%{Int64_pair.f1} in
           let* z = s2.%{Int64_pair.f2} in

           I64.(
             eq w (I64.v 3L)
             && eq x (I64.v 4L)
             && eq y (I64.v 1L)
             && eq z (I64.v 2L))))
       [()]

let for_loop start stop f =
  let open Repr in
  for_
    ~init:(I64.v start)
    ~pred:(fun i -> I64.le i (I64.v stop))
    ~step:(fun i -> I64.add i (I64.v 1L))
    f

let test_alloca_fixed_size_array () =
  Alcotest.(check (list bool)) "alloca_fixed_size_array" [true]
  @@ run_llvm_program1_unsafe
       ~verbose:true
       Ctypes.(void @-> returning bool)
       (let open Repr in
       fundecl
         ~name:"alloca_fixed_size_array"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.bool)
         ~local:
           Stack_frame.(
             arr Type_system.(arr (Size_cst 3L) int64) (I64.v 2L)
             @+ num Type_system.Int64_num @+ empty)
         ~body:(fun _self arr acc (_, ()) ->
           let* arr0 = get arr I64.zero in
           let* arr1 = get arr I64.one in
           let* _ = for_loop 0L 2L (fun i -> set arr0 i i) in
           let* _ = for_loop 0L 2L (fun i -> set arr1 i i) in
           let* _ = store acc I64.zero in
           let* _ =
             for_loop 0L 2L (fun i ->
                 for_loop 0L 1L (fun j ->
                     store acc (I64.add (load acc) (get (get arr j) i))))
           in
           I64.eq (load acc) (I64.v 6L)))
       [()]

module Bintree = struct
  open Repr
  open Type_system

  type bintree = { i : int64; a : (bintree ptr, [ `cst ]) arr }

  let r =
    fix @@ fun self ->
    empty_rec |+ field "i" int64 |+ field "a" (arr (Size_cst 2L) (ptr self))

  let t = seal r

  let (((), i), a) =
    projs r (fun { i; a } -> Cons_vec (a, Cons_vec (i, Nil_vec)))
end

let test_bintree () =
  Alcotest.(check (list unit)) "test_bintree" [()]
  @@ run_llvm_program1_unsafe
       ~verbose:true
       Ctypes.(void @-> returning void)
       (let open Repr in
       let open LLVM_state in
       let* bintree_sum =
         fundecl
           ~name:"bintre_sum"
           ~signature:Prototype.(Bintree.t @-> returning Type_system.int64)
           ~local:Stack_frame.empty
           ~body:(fun self (node, ()) ->
             let open Repr in
             let* a = node.%{Bintree.a} in
             let* na = get a I64.zero in
             let* nb = get a I64.one in
             let* va =
               cond (is_null na) @@ function
               | true -> I64.zero
               | false -> call self Prototype.[load na]
             in
             let* vb =
               cond (is_null nb) @@ function
               | true -> I64.zero
               | false -> call self Prototype.[load nb]
             in
             I64.add va vb)
       in
       fundecl
         ~name:"test_bintree"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.bool)
         ~local:
           Stack_frame.(
             strct Bintree.r @+ strct Bintree.r @+ strct Bintree.r @+ empty)
         ~body:(fun _self n1 n2 n3 (_, ()) ->
           let open Repr in
           let* _ = n1.%{Bintree.i} <- I64.v 41L in
           let* _ = n2.%{Bintree.i} <- I64.v 42L in
           let* _ = n3.%{Bintree.i} <- I64.v 43L in

           let* a1 = n1.%{Bintree.a} in
           let* _ = setaddr a1 (I64.v 0L) n2 in
           let* _ = setaddr a1 (I64.v 1L) n3 in
           let* a2 = n2.%{Bintree.a} in
           let* _ = set a1 (I64.v 0L) (null_ptr Bintree.t) in
           let* _ = set a1 (I64.v 1L) (null_ptr Bintree.t) in
           let* _ = set a2 (I64.v 0L) (null_ptr Bintree.t) in
           let* _ = set a2 (I64.v 1L) (null_ptr Bintree.t) in
           let* total = call bintree_sum Prototype.[n1] in
           I64.eq total (I64.v 126L)))
       [()]

let test_set_cst_array_in_struct () =
  Alcotest.(check (list bool)) "set_cst_array_in_struct" [true]
  @@ run_llvm_program1_unsafe
       ~verbose:true
       Ctypes.(void @-> returning bool)
       (let open Repr in
       fundecl
         ~name:"set_cst_array_in_struct"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.bool)
         ~local:
           Stack_frame.(
             strct Bintree.r @+ arr_cst (Type_system.ptr Bintree.t) 2L @+ empty)
         ~body:(fun _self strct arr (_, ()) ->
           let* a = strct.%{Bintree.a} in
           let* _ = setaddr a (I64.v 0L) strct in
           let* _ = setaddr a (I64.v 1L) strct in
           let* _ = set arr (I64.v 0L) (null_ptr Bintree.t) in
           let* _ = set arr (I64.v 1L) (null_ptr Bintree.t) in
           let* _ = strct.%{Bintree.a} <- arr in
           is_null (get a (I64.v 0L)) && is_null (get a (I64.v 1L))))
       [()]

let test_set_struct_in_struct () =
  let module Dummy = struct
    open Repr
    open Type_system

    type dummy = { dummy : Bintree.bintree }

    let r = empty_rec |+ field "dummy" Bintree.t

    let ((), dummy) = projs r (fun { dummy } -> Cons_vec (dummy, Nil_vec))
  end in
  Alcotest.(check (list bool)) "set_cst_struct_in_struct" [true]
  @@ run_llvm_program1_unsafe
       ~verbose:true
       Ctypes.(void @-> returning bool)
       (let open Repr in
       fundecl
         ~name:"set_cst_array_in_struct"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.bool)
         ~local:Stack_frame.(strct Bintree.r @+ strct Dummy.r @+ empty)
         ~body:(fun _self bintree dummy (_, ()) ->
           (bintree.%{Bintree.i} <- I64.v 0L) >> fun () ->
           let* a = bintree.%{Bintree.a} in
           set a (I64.v 0L) (null_ptr Bintree.t) >> fun () ->
           set a (I64.v 1L) (null_ptr Bintree.t) >> fun () ->
           (dummy.%{Dummy.dummy} <- bintree) >> fun () ->
           let* inner = dummy.%{Dummy.dummy} in
           let* a = inner.%{Bintree.a} in
           is_null (get a (I64.v 0L)) && is_null (get a (I64.v 1L))))
       [()]

let test_setaddr_struct_in_struct () =
  let module Dummy = struct
    open Repr
    open Type_system

    type dummy = { dummy : Bintree.bintree ptr }

    let r = empty_rec |+ field "dummy" (ptr Bintree.t)

    let ((), dummy) = projs r (fun { dummy } -> Cons_vec (dummy, Nil_vec))
  end in
  Alcotest.(check (list bool)) "set_cst_struct_in_struct" [true]
  @@ run_llvm_program1_unsafe
       ~verbose:true
       Ctypes.(void @-> returning bool)
       (let open Repr in
       fundecl
         ~name:"set_cst_array_in_struct"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.bool)
         ~local:Stack_frame.(strct Bintree.r @+ strct Dummy.r @+ empty)
         ~body:(fun _self bintree dummy (_, ()) ->
           (dummy.&{Dummy.dummy} <- bintree) >> fun () ->
           let* inner = dummy.%{Dummy.dummy} in
           let* a = (load inner).%{Bintree.a} in
           is_null (get a (I64.v 0L)) && is_null (get a (I64.v 1L))))
       [()]

let test_store_struct () =
  Alcotest.(check (list bool)) "store_struct" [true]
  @@ run_llvm_program1_unsafe
       ~verbose:true
       Ctypes.(void @-> returning bool)
       (let open Repr in
       fundecl
         ~name:"store_struct"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.bool)
         ~local:
           Stack_frame.(
             strct Int64_pair.r
             @+ arr_cst (Type_system.ptr Int64_pair.t) 2L
             @+ ptr Int64_pair.t @+ empty)
         ~body:(fun _self strct arr ptrptr (_, ()) ->
           let* _ = arr.&[I64.v 0L] <- strct in
           let* _ = store ptrptr arr.%[I64.v 0L] in
           let* ptr = load ptrptr in
           let* _ = store ptr strct in
           let* _ = strct.%{Int64_pair.f1} <- I64.v 42L in
           let* s' = load ptr in
           I64.eq strct.%{Int64_pair.f1} s'.%{Int64_pair.f1}))
       [()]

let test_store_cst_arr () =
  Alcotest.(check (list bool)) "store_cst_arr" [true]
  @@ run_llvm_program1_unsafe
       ~verbose:true
       Ctypes.(void @-> returning bool)
       (let open Repr in
       fundecl
         ~name:"store_cst_arr"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.bool)
         ~local:
           Stack_frame.(
             arr_cst Type_system.int64 2L
             @+ arr_cst Type_system.(ptr (arr (Size_cst 2L) int64)) 1L
             @+ ptr Type_system.(arr (Size_cst 2L) int64)
             @+ empty)
         ~body:(fun _self arr ptr_arr ptrptr (_, ()) ->
           let* _ = ptr_arr.&[I64.v 0L] <- arr in
           let* _ = store ptrptr ptr_arr.%[I64.v 0L] in
           let* ptr = load ptrptr in
           let* _ = store ptr arr in
           let* _ = arr.%[I64.v 0L] <- I64.v 42L in
           let* _ = arr.%[I64.v 1L] <- I64.v 43L in
           let* arr' = load ptr in
           I64.(eq (v 85L) (add arr'.%[I64.v 0L] arr'.%[I64.v 1L]))))
       [()]

let test_set_cst_arr () =
  Alcotest.(check (list int64)) "set_cst_arr" [510L]
  @@ run_llvm_program1_unsafe
       ~verbose:true
       Ctypes.(void @-> returning int64_t)
       (let open Repr in
       fundecl
         ~name:"set_cst_arr"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.int64)
         ~local:
           Stack_frame.(
             arr_cst Type_system.int64 2L
             @+ arr Type_system.(arr (Size_cst 2L) int64) (I64.v 3L)
             @+ arr_cst Type_system.(arr (Size_cst 2L) int64) 3L
             @+ num Type_system.Int64_num @+ empty)
         ~body:(fun _self arr_cst arr_cst_arr arr_cst_arr_cst acc (_, ()) ->
           for_loop 0L 1L (fun i -> arr_cst.%[i] <- I64.(add i (v 42L)))
           >> fun () ->
           for_loop 0L 2L (fun i ->
               (arr_cst_arr.%[i] <- arr_cst) >> fun () ->
               arr_cst_arr_cst.%[i] <- arr_cst)
           >> fun () ->
           store acc I64.zero >> fun () ->
           for_loop 0L 2L (fun i ->
               for_loop 0L 1L (fun j ->
                   store
                     acc
                     I64.(
                       add
                         (load acc)
                         (add arr_cst_arr.%[i].%[j] arr_cst_arr_cst.%[i].%[j]))))
           >> fun () -> load acc))
       [()]

let test_setaddr_struct_in_array () =
  Alcotest.(check (list bool)) "set_cst_array_in_struct" [true]
  @@ run_llvm_program1_unsafe
       ~verbose:true
       Ctypes.(void @-> returning bool)
       (let open Repr in
       fundecl
         ~name:"set_cst_array_in_struct"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.bool)
         ~local:
           Stack_frame.(
             strct Bintree.r
             @+ arr_cst (Type_system.ptr Bintree.t) 1L
             @+ arr (Type_system.ptr Bintree.t) I64.one
             @+ empty)
         ~body:(fun _self strct arr arr' (_, ()) ->
           let* _ = strct.%{Bintree.i} <- I64.v 33L in
           let* _ = arr.&[I64.v 0L] <- strct in
           let* _ = arr'.&[I64.v 0L] <- strct in
           let* s' = load @@ arr.%[I64.v 0L] in
           let* s'' = load @@ arr'.%[I64.v 0L] in
           I64.eq strct.%{Bintree.i} s'.%{Bintree.i}
           && I64.eq strct.%{Bintree.i} s''.%{Bintree.i}))
       [()]

let test_setaddr_array_in_array () =
  Alcotest.(check (list bool)) "set_cst_array_in_array" [true]
  @@ run_llvm_program1_unsafe
       ~verbose:true
       Ctypes.(void @-> returning bool)
       (let open Repr in
       fundecl
         ~name:"set_cst_array_in_array"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.bool)
         ~local:
           Stack_frame.(
             arr_cst Type_system.int64 2L
             @+ arr Type_system.(ptr (arr (Size_cst 2L) int64)) (I64.v 1L)
             @+ empty)
         ~body:(fun _self arr arr_arr (_, ()) ->
           let* _ = arr.%[I64.v 0L] <- I64.v 1L in
           let* _ = arr.%[I64.v 1L] <- I64.v 2L in
           let* _ = arr_arr.&[I64.v 0L] <- arr in
           let* a = load @@ arr_arr.%[I64.v 0L] in
           I64.eq a.%[I64.v 0L] (I64.v 1L) && I64.eq a.%[I64.v 1L] (I64.v 2L)))
       [()]

let test_record_copy () =
  let module R = struct
    open Repr
    open Type_system

    type t =
      { unit : unit;
        bool : bool;
        ptr : bool ptr;
        int : int64;
        strct : int64_pair;
        arr : (int64, [ `cst ]) arr
      }

    let r =
      empty_rec |+ field "unit" unit |+ field "bool" bool
      |+ field "ptr" (ptr bool)
      |+ field "int" int64 |+ field "strct" Int64_pair.t
      |+ field "arr" (arr (Size_cst 2L) int64)

    (* let t = seal r *)

    let (((((((), unit), bool), ptr), int), strct), arr) =
      projs r (fun { unit; bool; ptr; int; strct; arr } ->
          Cons_vec
            ( arr,
              Cons_vec
                ( strct,
                  Cons_vec
                    ( int,
                      Cons_vec (ptr, Cons_vec (bool, Cons_vec (unit, Nil_vec)))
                    ) ) ))
  end in
  Alcotest.(check (list bool)) "record_copy" [true]
  @@ run_llvm_program1_unsafe
       ~verbose:true
       Ctypes.(void @-> returning bool)
       (let open Repr in
       fundecl
         ~name:"record_copy"
         ~signature:Prototype.(Type_system.unit @-> returning Type_system.bool)
         ~local:
           Stack_frame.(
             strct Int64_pair.r
             @+ arr_cst Type_system.int64 2L
             @+ strct R.r @+ strct R.r @+ empty)
         ~body:(fun _self p arr s1 s2 (_, ()) ->
           seq (p.%{Int64_pair.f1} <- I64.zero) @@ fun () ->
           seq (p.%{Int64_pair.f2} <- I64.one) @@ fun () ->
           seq (s1.%{R.unit} <- unit) @@ fun () ->
           seq (s1.%{R.bool} <- tt) @@ fun () ->
           seq (s1.%{R.ptr} <- null_ptr Type_system.bool) @@ fun () ->
           seq (s1.%{R.int} <- I64.zero) @@ fun () ->
           seq (s1.%{R.strct} <- p) @@ fun () ->
           seq (s1.%{R.arr} <- arr) @@ fun () ->
           (* copy *)
           seq (s2.%{R.unit} <- s1.%{R.unit}) @@ fun () ->
           seq (s2.%{R.bool} <- s1.%{R.bool}) @@ fun () ->
           seq (s2.%{R.ptr} <- s1.%{R.ptr}) @@ fun () ->
           seq (s2.%{R.int} <- s1.%{R.int}) @@ fun () ->
           seq (s2.%{R.strct} <- s1.%{R.strct}) @@ fun () ->
           seq (s2.%{R.arr} <- s1.%{R.arr}) @@ fun () ->
           let check eq field = eq s1.%{field} s2.%{field} in
           let array_eq a1 a2 =
             let open I64 in
             eq a1.%[zero] a2.%[zero] && eq a1.%[one] a2.%[one]
           in
           check ( && ) R.bool && check ptr_eq R.ptr && check I64.eq R.int
           && check Int64_pair.eq R.strct
           && check array_eq R.arr))
       [()]

let () =
  let open Alcotest in
  run
    "llvm-codegen"
    [ ( "basic",
        [ test_case "store_unit" `Quick test_store_unit;
          test_case "store_bool" `Quick test_store_bool;
          test_case "fact" `Quick test_fact;
          test_case "nested_switch" `Quick test_nested_switch;
          test_case "nested_cond" `Quick test_nested_cond;
          test_case "struct_alloca" `Quick test_struct_alloca;
          test_case "struct_arg" `Quick test_struct_arg;
          test_case "struct_const_init" `Quick test_struct_const_init;
          test_case "array_arg" `Quick test_array_arg;
          test_case "alloca_struct_array" `Quick test_alloca_struct_array;
          test_case
            "alloca_struct_cst_array"
            `Quick
            test_alloca_struct_cst_array;
          test_case
            "alloca_fixed_size_array"
            `Quick
            test_alloca_fixed_size_array;
          test_case "bintree" `Quick test_bintree;
          test_case
            "set_cst_array_in_struct"
            `Quick
            test_set_cst_array_in_struct;
          test_case "set_struct_in_struct" `Quick test_set_struct_in_struct;
          test_case
            "setaddr_struct_in_struct"
            `Quick
            test_setaddr_struct_in_struct;
          test_case "store_struct" `Quick test_store_struct;
          test_case "store_cst_arr" `Quick test_store_cst_arr;
          test_case "set_cst_arr" `Quick test_set_cst_arr;
          test_case
            "setaddr_struct_in_array"
            `Quick
            test_setaddr_struct_in_array;
          test_case "setaddr_array_in_array" `Quick test_setaddr_array_in_array;
          test_case "record_copy" `Quick test_record_copy ] ) ]
