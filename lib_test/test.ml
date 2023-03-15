open Suplex
open Llvm_impl

(* Passing a unit, storing it in a local and loading it back doesn't fail. *)
let test_store_unit () =
  Alcotest.(check unit) "store_unit_with_runner" ()
  @@ (Exec.run
        Exec.(unit @-> returning unit)
        ( local Stack.unit @@ fun unit_ptr ->
          end_frame @@ fun _self _x ->
          let*! _ = store unit_ptr unit in
          load unit_ptr ))
       ()

(* Passing a bool, storing in a local and loading it back is the identity *)
let test_store_bool () =
  Alcotest.(check (list bool)) "store_bool" [true; false]
  @@ List.map
       (Exec.run
          Exec.(bool @-> returning bool)
          ( local Stack.bool @@ fun bool_ptr ->
            end_frame @@ fun _self x ->
            let*! _ = store bool_ptr x in
            load bool_ptr ))
       [true; false]

(* Iterative factorial implementation *)
let test_fact () =
  let rec fact_oracle n =
    if n = 0L then 1L else Int64.mul n (fact_oracle (Int64.pred n))
  in

  Alcotest.(check (list int64))
    "iterative_factorial"
    (List.map fact_oracle [1L; 2L; 3L; 4L; 5L])
  @@ (List.map
        (Exec.run
           Exec.(i64 @-> returning i64)
           ( local Stack.(num I64_num) @@ fun acc ->
             end_frame @@ fun _self n ->
             let*! _ = store acc I64.one in
             let*! _ =
               for_
                 ~init:(I64.v 1L)
                 ~pred:(fun i -> I64.le i n)
                 ~step:(fun i -> I64.add i (I64.v 1L))
                 (fun i -> store acc (I64.mul (load acc) i))
             in
             load acc )))
       [1L; 2L; 3L; 4L; 5L]

(* Iterative factorial implementation, using a while loop *)
let test_fact_while () =
  let rec fact_oracle n =
    if n = 0L then 1L else Int64.mul n (fact_oracle (Int64.pred n))
  in

  Alcotest.(check (list int64))
    "iterative_factorial_while"
    (List.map fact_oracle [1L; 2L; 3L; 4L; 5L])
  @@ (List.map
        (Exec.run
           Exec.(i64 @-> returning i64)
           ( local Stack.(num I64_num) @@ fun acc ->
             local Stack.(num I64_num) @@ fun i ->
             end_frame @@ fun _self n ->
             let*! _ = store acc I64.one in
             let*! _ = store i I64.one in
             let*! _ =
               while_
                 (fun () -> I64.le (load i) n)
                 (fun () ->
                   seq (store acc (I64.mul (load acc) (load i))) @@ fun () ->
                   store i (I64.add (load i) I64.one))
             in
             load acc )))
       [1L; 2L; 3L; 4L; 5L]

(* Iterative factorial implementation *)
let test_fact_with_foldi () =
  let rec fact_oracle n =
    if n = 0L then 1L else Int64.mul n (fact_oracle (Int64.pred n))
  in
  Alcotest.(check (list int64))
    "iterative_factorial_foldi"
    (List.map fact_oracle [1L; 2L; 3L; 4L; 5L])
  @@ List.map
       (Exec.run
          Exec.(i64 @-> returning i64)
          ( end_frame @@ fun _self n ->
            foldi
              ~init:(I64.v 1L)
              ~acc:I64.one
              ~pred:(fun i _ -> I64.le i n)
              ~step:(fun i -> I64.add i (I64.v 1L))
              (fun i acc -> I64.mul acc i) ))
       [1L; 2L; 3L; 4L; 5L]

(* Nested switches *)
let test_nested_switch () =
  Alcotest.(check (list int64))
    "nested_switch"
    [0L; 0L; 42L; 1789L; -1L]
    (List.map
       (Exec.run
          Exec.(i64 @-> returning i64)
          ( local Stack.(num I64_num) @@ fun local ->
            end_frame @@ fun _self x ->
            let*! _ = store local (I64.div x (I64.v 2L)) in
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
              ~default:(fun () -> I64.v (-1L)) ))
       [0L; 1L; 11L; 10L; 12L])

(* Nested conditionals *)
let test_nested_cond () =
  Alcotest.(check (list int64))
    "nested_cond"
    [0L; 0L; 42L; 1789L; 42L; 1789L]
    (List.map
       (Exec.run
          Exec.(i64 @-> returning i64)
          ( local Stack.(num I64_num) @@ fun local ->
            end_frame @@ fun _self x ->
            let*! _ = store local (I64.div x (I64.v 2L)) in
            let*! v = load local in
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
                                | true -> I64.v 42L | false -> I64.v 1789L))) ))
       [0L; 1L; 11L; 10L; 12L; 13L])

module Int64_pair = struct
  open Types

  type t

  let r = empty_rec |+ field "x" i64 |+ field "y" i64

  let t : t typ = seal r

  let (((), f1), f2) = projs r

  let eq s1 s2 = I64.eq s1.%{f1} s2.%{f1} && I64.eq s1.%{f2} s2.%{f2}
end

module Int64_pair_pair = struct
  open Types

  type t

  let r = empty_rec |+ field "u" Int64_pair.t |+ field "v" Int64_pair.t

  let t : t typ = seal r

  let (((), f1), f2) = projs r

  let eq s1 s2 =
    Int64_pair.eq s1.&{f1} s2.&{f1} && Int64_pair.eq s1.&{f2} s2.&{f2}
end

let ( >> ) = seq

(* Allocate a struct on the stack, store the arguments in
   its fields, load them back and return their sum. *)
let test_struct_alloca () =
  Alcotest.(check (list int64))
    "struct_alloca"
    [2L; 3L; 4L; 5L; 6L]
    (let f =
       Exec.run
         Exec.(i64 @-> i64 @-> returning i64)
         ( local Stack.(strct Int64_pair.r) @@ fun acc ->
           end_frame @@ fun _self x y ->
           let*! _ = acc.%{Int64_pair.f1} <- x in
           let*! _ = acc.%{Int64_pair.f2} <- y in
           I64.add acc.%{Int64_pair.f1} acc.%{Int64_pair.f2} )
     in
     List.map
       (fun (x, y) -> f x y)
       [(1L, 1L); (2L, 1L); (3L, 1L); (4L, 1L); (5L, 1L)])

(* Pass a pair of integers as argument and sum the fields. *)
let test_struct_arg () =
  let f =
    Exec.run
      Exec.(mallocd_strct Int64_pair.r (empty |+ i64 |+ i64) @-> returning i64)
    @@ end_frame (fun _self x -> I64.add x.%{Int64_pair.f1} x.%{Int64_pair.f2})
  in
  Alcotest.(check (list int64)) "struct_arg" [2L; 3L; 4L; 5L; 6L]
  @@ List.map f [[1L; 1L]; [2L; 1L]; [3L; 1L]; [4L; 1L]; [5L; 1L]]

(* Test construction of nested struct. *)
let test_nested_struct_arg () =
  let f =
    Exec.run
      Exec.(
        let pair = strct Int64_pair.r (empty |+ i64 |+ i64) in
        mallocd_strct Int64_pair_pair.r (empty |+ pair |+ pair)
        @-> returning i64)
      ( end_frame @@ fun _self x ->
        let*! pair1 = x.&{Int64_pair_pair.f1} in
        let*! pair2 = x.&{Int64_pair_pair.f2} in
        I64.add
          (I64.add pair1.%{Int64_pair.f1} pair1.%{Int64_pair.f2})
          (I64.add pair2.%{Int64_pair.f1} pair2.%{Int64_pair.f2}) )
  in
  Alcotest.(check (list int64)) "nested_struct_arg" [4L; 6L; 8L; 10L; 12L]
  @@ List.map
       f
       [ [[1L; 1L]; [1L; 1L]];
         [[2L; 1L]; [2L; 1L]];
         [[3L; 1L]; [3L; 1L]];
         [[4L; 1L]; [4L; 1L]];
         [[5L; 1L]; [5L; 1L]] ]

let test_struct_const_init () =
  let f =
    Exec.run
      Exec.(mallocd_strct Int64_pair.r (empty |+ i64 |+ i64) @-> returning i64)
      ( end_frame @@ fun _self x ->
        let*! _ = x.%{Int64_pair.f1} <- I64.v 42L in
        let*! _ = x.%{Int64_pair.f2} <- I64.v 43L in
        I64.add x.%{Int64_pair.f1} x.%{Int64_pair.f2} )
  in
  Alcotest.(check (list int64)) "struct_const_init" [85L; 85L; 85L; 85L; 85L]
  @@ List.map f [[1L; 1L]; [2L; 1L]; [3L; 1L]; [4L; 1L]; [5L; 1L]]

let for_loop start stop f =
  for_
    ~init:(I64.v start)
    ~pred:(fun i -> I64.le i (I64.v stop))
    ~step:(fun i -> I64.add i (I64.v 1L))
    f

let test_array_arg () =
  let f =
    Exec.run
      ~fname:"array_arg"
      Exec.(array_raw i64 @-> returning i64)
      ( local Stack.(num I64_num) @@ fun acc ->
        end_frame @@ fun _self x ->
        let*! _ = store acc I64.zero in
        let*! _ =
          for_loop 0L 4L (fun i -> store acc (I64.add (load acc) (get x i)))
        in
        load acc )
  in
  Alcotest.(check (list int64)) "array_arg" [5L; 10L]
  @@ List.map f [Array.to_seq [| 1L; 1L; 1L; 1L; 1L |]; Seq.init 5 Int64.of_int]

let test_cst_array_arg () =
  let f =
    Exec.run
      ~fname:"cst_array_arg"
      Exec.(array 5 i64 @-> returning i64)
      ( local Stack.(num I64_num) @@ fun acc ->
        end_frame @@ fun _self x ->
        let*! _ = store acc I64.zero in

        let*! _ =
          for_loop 0L 4L (fun i -> store acc (I64.add (load acc) (get x i)))
        in
        load acc )
  in
  Alcotest.(check (list int64)) "cst_array_arg" [5L; 10L]
  @@ List.map f [Array.to_seq [| 1L; 1L; 1L; 1L; 1L |]; Seq.init 5 Int64.of_int]

let test_alloca_struct_array () =
  let f =
    Exec.run
      Exec.(unit @-> returning bool)
      ( local Stack.(arr Int64_pair.t (I64.v 2L)) @@ fun arr ->
        local Stack.(strct Int64_pair.r) @@ fun tmp ->
        end_frame @@ fun _self _ ->
        let*! s1 = arr.&[I64.v 0L] in
        let*! s2 = arr.&[I64.v 1L] in

        let*! _ = s1.%{Int64_pair.f1} <- I64.v 1L in
        let*! _ = s1.%{Int64_pair.f2} <- I64.v 2L in

        let*! _ = s2.%{Int64_pair.f1} <- I64.v 3L in
        let*! _ = s2.%{Int64_pair.f2} <- I64.v 4L in

        let*! _ = tmp.%{Int64_pair.f1} <- s1.%{Int64_pair.f1} in
        let*! _ = tmp.%{Int64_pair.f2} <- s1.%{Int64_pair.f2} in

        let*! _ = set arr (I64.v 0L) (load s2) in
        let*! _ = set arr (I64.v 1L) (load tmp) in

        let*! w = s1.%{Int64_pair.f1} in
        let*! x = s1.%{Int64_pair.f2} in
        let*! y = s2.%{Int64_pair.f1} in
        let*! z = s2.%{Int64_pair.f2} in

        I64.(
          eq w (I64.v 3L)
          && eq x (I64.v 4L)
          && eq y (I64.v 1L)
          && eq z (I64.v 2L)) )
  in
  Alcotest.(check bool) "alloca_struct_array" true @@ f ()

let test_alloca_struct_cst_array () =
  let f =
    Exec.run
      Exec.(unit @-> returning bool)
      ( local Stack.(arr_cst Int64_pair.t 2L) @@ fun arr ->
        local Stack.(strct Int64_pair.r) @@ fun tmp ->
        end_frame @@ fun _self _ ->
        let*! s1 = getaddr arr (I64.v 0L) in
        let*! s2 = getaddr arr (I64.v 1L) in
        let*! _ = s1.%{Int64_pair.f1} <- I64.v 1L in
        let*! _ = s1.%{Int64_pair.f2} <- I64.v 2L in

        let*! _ = s2.%{Int64_pair.f1} <- I64.v 3L in
        let*! _ = s2.%{Int64_pair.f2} <- I64.v 4L in

        let*! _ = tmp.%{Int64_pair.f1} <- s1.%{Int64_pair.f1} in
        let*! _ = tmp.%{Int64_pair.f2} <- s1.%{Int64_pair.f2} in

        let*! _ = set arr (I64.v 0L) (load s2) in
        let*! _ = set arr (I64.v 1L) (load tmp) in

        let*! w = s1.%{Int64_pair.f1} in
        let*! x = s1.%{Int64_pair.f2} in
        let*! y = s2.%{Int64_pair.f1} in
        let*! z = s2.%{Int64_pair.f2} in

        I64.(
          eq w (I64.v 3L)
          && eq x (I64.v 4L)
          && eq y (I64.v 1L)
          && eq z (I64.v 2L)) )
  in
  Alcotest.(check bool) "alloca_struct_cst_array" true @@ f ()

let test_alloca_fixed_size_array () =
  let f =
    Exec.run
      Exec.(unit @-> returning bool)
      ( local Stack.(arr Types.(arr_cst i64 3L) (I64.v 2L)) @@ fun arr ->
        local Stack.(num I64_num) @@ fun acc ->
        end_frame @@ fun _self _ ->
        let*! arr0 = get arr I64.zero in
        let*! arr1 = get arr I64.one in
        let*! _ = for_loop 0L 2L (fun i -> set arr0 i i) in
        let*! _ = for_loop 0L 2L (fun i -> set arr1 i i) in
        let*! _ = store acc I64.zero in
        let*! _ =
          for_loop 0L 2L (fun i ->
              for_loop 0L 1L (fun j ->
                  store acc (I64.add (load acc) (get (get arr j) i))))
        in
        I64.eq (load acc) (I64.v 6L) )
  in
  Alcotest.(check bool) "alloca_fixed_size_array" true @@ f ()

module Bintree = struct
  open Types

  type bintree

  let r =
    fix @@ fun self ->
    empty_rec |+ field "i" i64 |+ field "a" (arr_cst (ptr self) 2L)

  let t : bintree typ = seal r

  let (((), i), a) = projs r
end

let test_bintree () =
  let open K in
  let program =
    let* bintree_sum =
      fundecl
        "bintree_sum"
        Types.(ptr Bintree.t @-> returning i64)
        ( end_frame @@ fun self node ->
          cond (is_null node) @@ function
          | true -> I64.zero
          | false ->
              let*! a = node.%{Bintree.a} in
              let*! na = get a I64.zero in
              let*! nb = get a I64.one in
              let*! va = call self na in
              let*! vb = call self nb in
              I64.add (I64.add node.%{Bintree.i} va) vb )
    in
    fundecl
      "test_bintree"
      Types.(unit @-> returning bool)
      ( local Stack.(strct Bintree.r) @@ fun n1 ->
        local Stack.(strct Bintree.r) @@ fun n2 ->
        local Stack.(strct Bintree.r) @@ fun n3 ->
        end_frame @@ fun _self _unit ->
        let*! _ = n1.%{Bintree.i} <- I64.v 41L in
        let*! _ = n2.%{Bintree.i} <- I64.v 42L in
        let*! _ = n3.%{Bintree.i} <- I64.v 43L in

        let*! a1 = n1.%{Bintree.a} in
        let*! _ = set a1 (I64.v 0L) n2 in
        let*! _ = set a1 (I64.v 1L) n3 in
        let*! a2 = n2.%{Bintree.a} in
        let*! _ = set a2 (I64.v 0L) (null_ptr Bintree.t) in
        let*! _ = set a2 (I64.v 1L) (null_ptr Bintree.t) in
        let*! a3 = n3.%{Bintree.a} in
        let*! _ = set a3 (I64.v 0L) (null_ptr Bintree.t) in
        let*! _ = set a3 (I64.v 1L) (null_ptr Bintree.t) in
        let*! total = call bintree_sum n1 in
        I64.eq total (I64.v 126L) )
  in
  let f = Exec.(run_program program (unit @-> returning bool)) in
  Alcotest.(check bool) "test_bintree" true @@ f ()

let test_set_cst_array_in_struct () =
  let f =
    Exec.run
      Exec.(unit @-> returning bool)
      ( local Stack.(strct Bintree.r) @@ fun strct ->
        local Stack.(arr_cst (Types.ptr Bintree.t) 2L) @@ fun arr ->
        end_frame @@ fun _self _ ->
        let*! a = strct.%{Bintree.a} in
        let*! _ = set a (I64.v 0L) strct in
        let*! _ = set a (I64.v 1L) strct in
        let*! _ = set arr (I64.v 0L) (null_ptr Bintree.t) in
        let*! _ = set arr (I64.v 1L) (null_ptr Bintree.t) in
        let*! _ = strct.%{Bintree.a} <- arr in
        is_null (get a (I64.v 0L)) && is_null (get a (I64.v 1L)) )
  in
  Alcotest.(check bool) "set_cst_array_in_struct" true @@ f ()

let test_set_struct_in_struct () =
  let module Dummy = struct
    open Types

    type dummy

    let r = empty_rec |+ field "dummy" Bintree.t

    let (_ : dummy typ) = seal r

    let ((), dummy) = projs r
  end in
  let f =
    Exec.run
      Exec.(unit @-> returning bool)
      ( local Stack.(strct Bintree.r) @@ fun bintree ->
        local Stack.(strct Dummy.r) @@ fun dummy ->
        end_frame @@ fun _self _ ->
        (bintree.%{Bintree.i} <- I64.v 0L) >> fun () ->
        let*! a = bintree.%{Bintree.a} in
        set a (I64.v 0L) (null_ptr Bintree.t) >> fun () ->
        set a (I64.v 1L) (null_ptr Bintree.t) >> fun () ->
        (dummy.%{Dummy.dummy} <- load bintree) >> fun () ->
        let*! inner = dummy.&{Dummy.dummy} in
        let*! a = inner.%{Bintree.a} in
        is_null (get a (I64.v 0L)) && is_null (get a (I64.v 1L)) )
  in
  Alcotest.(check bool) "set_cst_struct_in_struct" true @@ f ()

let test_setaddr_struct_in_struct () =
  let module Dummy = struct
    open Types

    type dummy

    let r = empty_rec |+ field "dummy" (ptr Bintree.t)

    let (_ : dummy typ) = seal r

    let ((), dummy) = projs r
  end in
  let f =
    Exec.run
      Exec.(unit @-> returning bool)
      ( local Stack.(strct Bintree.r) @@ fun bintree ->
        local Stack.(strct Dummy.r) @@ fun dummy ->
        end_frame @@ fun _self _ ->
        let*! a = bintree.%{Bintree.a} in
        set a (I64.v 0L) (null_ptr Bintree.t) >> fun () ->
        set a (I64.v 1L) (null_ptr Bintree.t) >> fun () ->
        (dummy.%{Dummy.dummy} <- bintree) >> fun () ->
        let*! inner = dummy.%{Dummy.dummy} in
        let*! a = inner.%{Bintree.a} in
        is_null (get a (I64.v 0L)) && is_null (get a (I64.v 1L)) )
  in
  Alcotest.(check bool) "setaddr_struct_in_struct" true (f ())

let test_store_struct () =
  let f =
    Exec.run
      Exec.(unit @-> returning bool)
      ( local Stack.(strct Int64_pair.r) @@ fun strct ->
        local Stack.(arr_cst (Types.ptr Int64_pair.t) 2L) @@ fun arr ->
        local Stack.(ptr Int64_pair.t) @@ fun ptrptr ->
        end_frame @@ fun _self _ ->
        let*! _ = arr.%[I64.v 0L] <- strct in
        let*! _ = store ptrptr arr.%[I64.v 0L] in
        let*! ptr = load ptrptr in
        let*! _ = store ptr (load strct) in
        let*! _ = strct.%{Int64_pair.f1} <- I64.v 42L in
        I64.eq strct.%{Int64_pair.f1} ptr.%{Int64_pair.f1} )
  in
  Alcotest.(check bool) "store_struct" true (f ())

let test_store_cst_arr () =
  let f =
    Exec.run
      Exec.(unit @-> returning bool)
      ( local Stack.(arr_cst Types.i64 2L) @@ fun arr ->
        local Stack.(arr_cst Types.(ptr (arr_cst i64 2L)) 1L) @@ fun ptr_arr ->
        local Stack.(ptr Types.(arr_cst i64 2L)) @@ fun ptrptr ->
        end_frame (fun _self _ ->
            (* Initialize fixed array of ints *)
            let*! _ = arr.%[I64.v 0L] <- I64.v 42L in
            let*! _ = arr.%[I64.v 1L] <- I64.v 43L in
            (* Store the address of [arr] in [ptr_arr] *)
            let*! _ = ptr_arr.&[I64.v 0L] <- arr in
            (* Store the contents of [ptr_arr] in [ptr_ptr] *)
            let*! _ = store ptrptr ptr_arr.%[I64.v 0L] in
            let*! arr' = load (load ptrptr) in
            I64.(eq (v 85L) (add arr'.%[I64.v 0L] arr'.%[I64.v 1L]))) )
  in
  Alcotest.(check bool) "store_cst_arr" true @@ f ()

let test_set_cst_arr () =
  let f =
    Exec.run
      Exec.(unit @-> returning i64)
      (let*:: arr_cst = Stack.(arr_cst Types.i64 2L) in
       let*:: arr_cst_arr = Stack.(arr Types.(arr_cst i64 2L) (I64.v 3L)) in
       let*:: arr_cst_arr_cst = Stack.(arr_cst Types.(arr_cst i64 2L) 3L) in
       let*:: acc = Stack.(num I64_num) in
       end_frame @@ fun _self _ ->
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
       >> fun () -> load acc)
  in
  Alcotest.(check int64) "set_cst_arr" 510L (f ())

let test_setaddr_struct_in_array () =
  let f =
    Exec.run
      Exec.(unit @-> returning bool)
      (let*:: strct = Stack.(strct Bintree.r) in
       let*:: arr = Stack.(arr_cst (Types.ptr Bintree.t) 1L) in
       let*:: arr' = Stack.(arr (Types.ptr Bintree.t) I64.one) in
       end_frame @@ fun _self _ ->
       let*! _ = strct.%{Bintree.i} <- I64.v 33L in
       let*! _ = arr.%[I64.v 0L] <- strct in
       let*! _ = arr'.%[I64.v 0L] <- strct in
       let*! s' = arr.%[I64.v 0L] in
       let*! s'' = arr'.%[I64.v 0L] in
       I64.eq strct.%{Bintree.i} s'.%{Bintree.i}
       && I64.eq strct.%{Bintree.i} s''.%{Bintree.i})
  in
  Alcotest.(check bool) "set_cst_array_in_struct" true @@ f ()

let test_setaddr_array_in_array () =
  let f =
    Exec.run
      Exec.(unit @-> returning bool)
      (let*:: arr = Stack.(arr_cst Types.i64 2L) in
       let*:: arr_arr = Stack.(arr Types.(ptr (arr_cst i64 2L)) (I64.v 1L)) in
       end_frame @@ fun _self _ ->
       let*! _ = arr.%[I64.v 0L] <- I64.v 1L in
       let*! _ = arr.%[I64.v 1L] <- I64.v 2L in
       let*! _ = arr_arr.&[I64.v 0L] <- arr in
       let*! a = load @@ arr_arr.%[I64.v 0L] in
       I64.eq a.%[I64.v 0L] (I64.v 1L) && I64.eq a.%[I64.v 1L] (I64.v 2L))
  in
  Alcotest.(check bool) "set_cst_array_in_array" true (f ())

let test_record_copy () =
  let module R = struct
    open Types

    type t

    let r =
      empty_rec |+ field "unit" unit |+ field "bool" bool
      |+ field "ptr" (ptr bool)
      |+ field "int" i64 |+ field "strct" Int64_pair.t
      |+ field "arr" (arr_cst i64 2L)

    let (_ : t typ) = seal r

    let (((((((), unit), bool), ptr), int), strct), arr) = projs r
  end in
  let f =
    Exec.run
      Exec.(unit @-> returning bool)
      (let*:: p = Stack.(strct Int64_pair.r) in
       let*:: arr = Stack.(arr_cst Types.i64 2L) in
       let*:: s1 = Stack.(strct R.r) in
       let*:: s2 = Stack.(strct R.r) in
       end_frame @@ fun _self _ ->
       seq (p.%{Int64_pair.f1} <- I64.zero) @@ fun () ->
       seq (p.%{Int64_pair.f2} <- I64.one) @@ fun () ->
       seq (s1.%{R.unit} <- unit) @@ fun () ->
       seq (s1.%{R.bool} <- tt) @@ fun () ->
       seq (s1.%{R.ptr} <- null_ptr Types.bool) @@ fun () ->
       seq (s1.%{R.int} <- I64.zero) @@ fun () ->
       seq (s1.%{R.strct} <- load p) @@ fun () ->
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
       && Int64_pair.eq s1.&{R.strct} s2.&{R.strct}
       && check array_eq R.arr)
  in
  Alcotest.(check bool) "record_copy" true @@ f ()

let test_fail () =
  let f =
    Exec.run
      Exec.(unit @-> returning unit)
      (end_frame @@ fun _self _arg -> fail "test_fail")
  in
  Alcotest.check_raises "test_fail" (Failure "test_fail") f

let test_fail_branch1 () =
  let f =
    Exec.run
      Exec.(bool @-> returning unit)
      ( end_frame @@ fun _self arg ->
        cond arg (function false -> fail "false" | true -> unit) )
  in
  Alcotest.check_raises "test_fail_if_false" (Failure "false") (fun () ->
      f false)

let test_fail_branch2 () =
  let f =
    Exec.run
      Exec.(bool @-> returning unit)
      ( end_frame @@ fun _self arg ->
        cond arg (function true -> fail "true" | false -> unit) )
  in
  Alcotest.check_raises "test_fail_if_false" (Failure "true") (fun () -> f true)

let test_fail_branch3 () =
  let f =
    Exec.run
      Exec.(bool @-> returning unit)
      ( end_frame @@ fun _self arg ->
        cond arg (function true -> fail "true" | false -> fail "false") )
  in
  Alcotest.check_raises "test_fail_if_false" (Failure "true") (fun () -> f true)

(* Nested switches *)
let test_switch_with_fail () =
  let f =
    Exec.run
      Exec.(i64 @-> returning i64)
      ( end_frame @@ fun _self x ->
        switch_i64
          x
          ~cases:[| (0L, fun () -> I64.zero); (5L, fun () -> I64.one) |]
          ~default:(fun () -> fail "default") )
  in
  Alcotest.check_raises "failing_switch" (Failure "default") (fun () ->
      ignore (f 8L))

(* Nested switches *)
let test_switch_all_fail () =
  let f =
    Exec.run
      Exec.(i64 @-> returning i64)
      ( end_frame @@ fun _self x ->
        switch_i64
          x
          ~cases:[| (0L, fun () -> fail "0"); (5L, fun () -> fail "1") |]
          ~default:(fun () -> fail "default") )
  in
  Alcotest.check_raises "failing_switch" (Failure "default") (fun () ->
      ignore (f 8L))

let test_bigarray :
    type ba s o.
    (module Numerical with type t = s) ->
    (module Exec.BA with type elt = s and type s = ba) ->
    (ba ptr Types.m, (o, 'elt, Bigarray.c_layout) Bigarray.Array1.t) Exec.rel ->
    (s Types.m, o) Exec.rel ->
    (string -> o -> o -> unit) ->
    (o, 'elt, Bigarray.c_layout) Bigarray.Array1.t ->
    o ->
    unit =
 fun (module N : Numerical with type t = s)
     (module BA : Exec.BA with type elt = s and type s = ba)
     (rel :
       (ba ptr Types.m, (o, 'elt, Bigarray.c_layout) Bigarray.Array1.t) Exec.rel)
     (retrel : (s Types.m, o) Exec.rel)
     check
     v
     expected ->
  let f =
    Exec.run
      Exec.(rel @-> returning retrel)
      ( end_frame @@ fun _self arg ->
        let*! dim = arg.%{BA.dim} in
        let*! data = arg.%{BA.data} in
        foldi
          ~init:I64.zero
          ~acc:N.one
          ~pred:(fun i _ -> I64.lt i dim)
          ~step:(fun i -> I64.add i I64.one)
          (fun i acc -> N.mul acc data.%[i]) )
  in
  check "test_bigarray" expected @@ f v

let test_bigarray_i64 () =
  test_bigarray
    (module I64)
    (module Exec.I64_ba)
    Exec.bigarray_i64
    Exec.i64
    Alcotest.(check int64)
    (Bigarray.Array1.of_array
       Bigarray.int64
       Bigarray.c_layout
       [| 1L; 2L; 3L; 4L; 5L |])
    120L

let test_bigarray_i32 () =
  test_bigarray
    (module I32)
    (module Exec.I32_ba)
    Exec.bigarray_i32
    Exec.i32
    Alcotest.(check int32)
    (Bigarray.Array1.of_array
       Bigarray.int32
       Bigarray.c_layout
       [| 1l; 2l; 3l; 4l; 5l |])
    120l

let test_bigarray_i16 () =
  test_bigarray
    (module I16)
    (module Exec.I16_ba)
    Exec.bigarray_i16
    Exec.i16
    Alcotest.(check int)
    (Bigarray.Array1.of_array
       Bigarray.int16_signed
       Bigarray.c_layout
       [| 1; 2; 3; 4; 5 |])
    120

let test_bigarray_i8 () =
  test_bigarray
    (module I8)
    (module Exec.I8_ba)
    Exec.bigarray_i8
    Exec.i8
    Alcotest.(check int)
    (Bigarray.Array1.of_array
       Bigarray.int8_signed
       Bigarray.c_layout
       [| 1; 2; 3; 4; 5 |])
    120

let test_bigarray_f64 () =
  test_bigarray
    (module F64)
    (module Exec.F64_ba)
    Exec.bigarray_f64
    Exec.f64
    Alcotest.(check (float 0.00001))
    (Bigarray.Array1.of_array
       Bigarray.float64
       Bigarray.c_layout
       [| 1.; 2.; 3.; 4.; 5. |])
    120.

let test_bigarray_f32 () =
  test_bigarray
    (module F32)
    (module Exec.F32_ba)
    Exec.bigarray_f32
    Exec.f32
    Alcotest.(check (float 0.00001))
    (Bigarray.Array1.of_array
       Bigarray.float32
       Bigarray.c_layout
       [| 1.; 2.; 3.; 4.; 5. |])
    120.

let test_malloc () =
  let f =
    Exec.run
      Exec.(unit @-> returning i64)
      ( end_frame @@ fun _self _arg ->
        let module PP = Int64_pair_pair in
        let module P = Int64_pair in
        let*! strct = malloc PP.t in
        let*! _ = strct.&{PP.f1}.%{P.f1} <- I64.v 10L in
        let*! _ = strct.&{PP.f1}.%{P.f2} <- I64.v 11L in
        let*! _ = strct.&{PP.f2}.%{P.f1} <- I64.v 13L in
        let*! _ = strct.&{PP.f2}.%{P.f2} <- I64.v 14L in
        let ( + ) = I64.add in
        let*! sum =
          strct.&{PP.f1}.%{P.f1}
          + strct.&{PP.f1}.%{P.f2}
          + strct.&{PP.f2}.%{P.f1}
          + strct.&{PP.f2}.%{P.f2}
        in
        let*! _ = free strct in
        sum )
  in
  Alcotest.(check int64) "test_malloc" 48L @@ f ()

let test_malloc_array () =
  let f =
    Exec.run
      Exec.(unit @-> returning i64)
      (let*:: acc = Stack.(num Suplex.I64_num) in
       end_frame @@ fun _self _arg ->
       let module PP = Int64_pair_pair in
       let module P = Int64_pair in
       let*! _ = store acc I64.zero in
       let*! arr = malloc_array PP.t (I64.v 4L) in
       let*! _ =
         for_loop 0L 3L (fun i ->
             let*! strct = arr.&[i] in
             let*! _ = strct.&{PP.f1}.%{P.f1} <- I64.v 10L in
             let*! _ = strct.&{PP.f1}.%{P.f2} <- I64.v 11L in
             let*! _ = strct.&{PP.f2}.%{P.f1} <- I64.v 13L in
             let*! _ = strct.&{PP.f2}.%{P.f2} <- I64.v 14L in
             let ( + ) = I64.add in
             let*! sum =
               strct.&{PP.f1}.%{P.f1}
               + strct.&{PP.f1}.%{P.f2}
               + strct.&{PP.f2}.%{P.f1}
               + strct.&{PP.f2}.%{P.f2}
             in
             store acc (load acc + sum))
       in
       let*! _ = free_array arr in
       load acc)
  in
  Alcotest.(check int64) "test_malloc" (Int64.mul 4L 48L) @@ f ()

let test_opaque_mallocd_strct () =
  let open K in
  let (((), alloc), sum) =
    Exec.run_module
    @@ let* alloc_int64_pair =
         fundecl
           "alloc"
           Types.(unit @-> returning (ptr Int64_pair.t))
           ( end_frame @@ fun _self _ ->
             let*! pair = malloc Int64_pair.t in
             let*! _ = pair.%{Int64_pair.f1} <- I64.v 12L in
             let*! _ = pair.%{Int64_pair.f2} <- I64.v 30L in
             pair )
       in
       let* sum_int64_pair =
         fundecl
           "sum"
           Types.(ptr Int64_pair.t @-> returning i64)
           ( end_frame @@ fun _self pair ->
             let*! x = pair.%{Int64_pair.f1} in
             let*! y = pair.%{Int64_pair.f2} in
             I64.add x y )
       in
       return
         Exec.(
           empty_module
           |> add_fdecl
                alloc_int64_pair
                (unit @-> returning (opaque_mallocd_strct Int64_pair.r))
           |> add_fdecl
                sum_int64_pair
                (opaque_mallocd_strct Int64_pair.r @-> returning i64))
  in
  Alcotest.(check int64) "test_opaque" 42L @@ sum (alloc ())

let test_global_array () =
  let f =
    Exec.run
      Exec.(unit @-> returning f64)
      (let*:: acc = Stack.(num Suplex.F64_num) in
       end_frame @@ fun _self _arg ->
       let*! _ = store acc F64.zero in
       let*! arr = global_array (module F64) [| 1.0; 2.0; 3.0; 4.0 |] in
       let*! _ =
         for_loop 0L 3L (fun i ->
             let*! v = arr.%[i] in
             store acc (F64.add (load acc) v))
       in
       load acc)
  in
  Alcotest.(check (float 0.01)) "test_global_array" 10. @@ f ()

(* let () =
 *   let ty = Ctypes.(void @-> returning i64_t) in
 *   let _lib =
 *     Dl.dlopen ~filename:"/tmp/libtest" ~flags:Dl.[RTLD_NOW; RTLD_GLOBAL]
 *   in
 *   let f = Foreign.foreign "f" ty in
 *   Format.printf "foreign function called with result %Ld@." (f ()) *)

let () =
  let open Alcotest in
  run
    "llvm-codegen"
    [ ( "basic",
        [ test_case "store_unit" `Quick test_store_unit;
          test_case "store_bool" `Quick test_store_bool;
          test_case "fact" `Quick test_fact;
          test_case "fact_while" `Quick test_fact_while;
          test_case "fact_foldi" `Quick test_fact_with_foldi;
          test_case "nested_switch" `Quick test_nested_switch;
          test_case "nested_cond" `Quick test_nested_cond;
          test_case "struct_alloca" `Quick test_struct_alloca;
          test_case "struct_arg" `Quick test_struct_arg;
          test_case "nested_struct_arg" `Quick test_nested_struct_arg;
          test_case "struct_const_init" `Quick test_struct_const_init;
          test_case "array_arg" `Quick test_array_arg;
          test_case "cst_array_arg" `Quick test_cst_array_arg;
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
          test_case "record_copy" `Quick test_record_copy;
          test_case "test_fail" `Quick test_fail;
          test_case "test_fail_branch_1" `Quick test_fail_branch1;
          test_case "test_fail_branch_2" `Quick test_fail_branch2;
          test_case "test_fail_branch_3" `Quick test_fail_branch3;
          test_case "test_switch_with_fail" `Quick test_switch_with_fail;
          test_case "test_switch_all_fail" `Quick test_switch_all_fail;
          test_case "test_bigarray_i64" `Quick test_bigarray_i64;
          test_case "test_bigarray_i32" `Quick test_bigarray_i32;
          test_case "test_bigarray_i16" `Quick test_bigarray_i16;
          test_case "test_bigarray_i16" `Quick test_bigarray_i8;
          test_case "test_bigarray_f64" `Quick test_bigarray_f64;
          test_case "test_bigarray_f32" `Quick test_bigarray_f32;
          test_case "test_malloc" `Quick test_malloc;
          test_case "test_malloc_array" `Quick test_malloc_array;
          test_case "test_opaque_strct" `Quick test_opaque_mallocd_strct;
          test_case "test_global_array" `Quick test_global_array ] ) ]
