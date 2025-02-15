open Suplex

(* Passing a unit, storing it in a local and ~*ing it back doesn't fail. *)
let test_store_unit () =
  Alcotest.(check unit) "store_unit_with_runner" ()
  @@ (Run.run
        Run.(unit @-> returning unit)
        ( local Stack.unit @@ fun unit_ptr ->
          end_frame @@ fun _self _x ->
          let* _ = unit_ptr <-- unit in
          ~*unit_ptr ))
       ()

(* Passing a bool, storing in a local and ~*ing it back is the identity *)
let test_store_bool () =
  Alcotest.(check (list bool)) "store_bool" [true; false]
  @@ List.map
       (Run.run
          Run.(bool @-> returning bool)
          ( local Stack.bool @@ fun bool_ptr ->
            end_frame @@ fun _self x ->
            let* _ = bool_ptr <-- x in
            ~*bool_ptr ))
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
        (Run.run
           Run.(i64 @-> returning i64)
           ( local Stack.(num I64_num) @@ fun acc ->
             end_frame @@ fun _self n ->
             let* _ = acc <-- I64.v 1L in
             let* _ =
               for_
                 ~init:(I64.v 1L)
                 ~pred:(fun i -> I64.le i n)
                 ~step:(fun i -> I64.add i (I64.v 1L))
                 (fun i -> acc <-- I64.mul ~*acc i)
             in
             ~*acc )))
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
        (Run.run
           Run.(i64 @-> returning i64)
           ( local Stack.(num I64_num) @@ fun acc ->
             local Stack.(num I64_num) @@ fun i ->
             end_frame @@ fun _self n ->
             let* _ = acc <-- I64.one in
             let* _ = i <-- I64.one in
             let* _ =
               while_
                 (I64.le ~*i n)
                 (block [acc <-- I64.mul ~*acc ~*i; i <-- I64.add ~*i I64.one])
             in
             ~*acc )))
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
       (Run.run
          Run.(i64 @-> returning i64)
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
       (Run.run
          Run.(i64 @-> returning i64)
          ( local Stack.(num I64_num) @@ fun local ->
            end_frame @@ fun _self x ->
            let* _ = local <-- I64.div x (I64.v 2L) in
            switch
              ~*local
              [ (0L, I64.zero);
                (5L, switch x [(11L, I64.v 42L)] ~default:(I64.v 1789L)) ]
              ~default:(I64.v (-1L)) ))
       [0L; 1L; 11L; 10L; 12L])

(* Nested conditionals *)
let test_nested_cond () =
  Alcotest.(check (list int64))
    "nested_cond"
    [0L; 0L; 42L; 1789L; 42L; 1789L]
    (List.map
       (Run.run
          Run.(i64 @-> returning i64)
          ( local Stack.(num I64_num) @@ fun local ->
            end_frame @@ fun _self x ->
            let* _ = local <-- I64.div x (I64.v 2L) in
            let* v = ~*local in
            if_
              (I64.eq v I64.zero)
              I64.zero
              (if_
                 (I64.eq v (I64.v 5L))
                 (if_ (I64.eq x (I64.v 11L)) (I64.v 42L) (I64.v 1789L))
                 (if_ (I64.eq x (I64.v 12L)) (I64.v 42L) (I64.v 1789L))) ))
       [0L; 1L; 11L; 10L; 12L; 13L])

module Int64_pair = struct
  open Types
  open Syntax

  type t

  let r = empty_rec |+ field "x" i64 |+ field "y" i64

  let t : t typ = seal r

  let (((), f1), f2) = projs r

  let eq s1 s2 = I64.eq s1.%{f1} s2.%{f1} &&& I64.eq s1.%{f2} s2.%{f2}
end

module Int64_pair_pair = struct
  open Types
  open Syntax

  type t

  let r = empty_rec |+ field "u" Int64_pair.t |+ field "v" Int64_pair.t

  let t : t typ = seal r

  let (((), f1), f2) = projs r

  let eq s1 s2 =
    Int64_pair.eq s1.%&{f1} s2.%&{f1} &&& Int64_pair.eq s1.%&{f2} s2.%&{f2}
end

(* Allocate a struct on the stack, store the arguments in
   its fields, ~* them back and return their sum. *)
let test_struct_alloca () =
  Alcotest.(check (list int64))
    "struct_alloca"
    [2L; 3L; 4L; 5L; 6L]
    (let f =
       Run.run
         Run.(i64 @-> i64 @-> returning i64)
         ( local Stack.(strct Int64_pair.r) @@ fun acc ->
           end_frame @@ fun _self x y ->
           let* _ = acc.%{Int64_pair.f1} <- x in
           let* _ = acc.%{Int64_pair.f2} <- y in
           I64.add acc.%{Int64_pair.f1} acc.%{Int64_pair.f2} )
     in
     List.map
       (fun (x, y) -> f x y)
       [(1L, 1L); (2L, 1L); (3L, 1L); (4L, 1L); (5L, 1L)])

(* Pass a pair of integers as argument and sum the fields. *)
let test_struct_arg () =
  let f =
    Run.run
      Run.(mallocd_strct Int64_pair.r (empty |+ i64 |+ i64) @-> returning i64)
    @@ end_frame (fun _self x -> I64.add x.%{Int64_pair.f1} x.%{Int64_pair.f2})
  in
  Alcotest.(check (list int64)) "struct_arg" [2L; 3L; 4L; 5L; 6L]
  @@ List.map f [[1L; 1L]; [2L; 1L]; [3L; 1L]; [4L; 1L]; [5L; 1L]]

(* Test construction of nested struct. *)
let test_nested_struct_arg () =
  let f =
    Run.run
      Run.(
        let pair = strct Int64_pair.r (empty |+ i64 |+ i64) in
        mallocd_strct Int64_pair_pair.r (empty |+ pair |+ pair)
        @-> returning i64)
      ( end_frame @@ fun _self x ->
        let* pair1 = x.%&{Int64_pair_pair.f1} in
        let* pair2 = x.%&{Int64_pair_pair.f2} in
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
    Run.run
      Run.(mallocd_strct Int64_pair.r (empty |+ i64 |+ i64) @-> returning i64)
      ( end_frame @@ fun _self x ->
        let* _ = x.%{Int64_pair.f1} <- I64.v 42L in
        let* _ = x.%{Int64_pair.f2} <- I64.v 43L in
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
    Run.run
      ~fname:"array_arg"
      Run.(array_raw i64 @-> returning i64)
      ( local Stack.(num I64_num) @@ fun acc ->
        end_frame @@ fun _self x ->
        let* _ = acc <-- I64.zero in
        let* _ = for_loop 0L 4L (fun i -> acc <-- I64.add ~*acc x.%[i]) in
        ~*acc )
  in
  Alcotest.(check (list int64)) "array_arg" [5L; 10L]
  @@ List.map f [Array.to_seq [| 1L; 1L; 1L; 1L; 1L |]; Seq.init 5 Int64.of_int]

let test_cst_array_arg () =
  let f =
    Run.run
      ~fname:"cst_array_arg"
      Run.(array 5 i64 @-> returning i64)
      ( local Stack.(num I64_num) @@ fun acc ->
        end_frame @@ fun _self x ->
        let* _ = acc <-- I64.zero in
        let* _ = for_loop 0L 4L (fun i -> acc <-- I64.add ~*acc x.%[i]) in
        ~*acc )
  in
  Alcotest.(check (list int64)) "cst_array_arg" [5L; 10L]
  @@ List.map f [Array.to_seq [| 1L; 1L; 1L; 1L; 1L |]; Seq.init 5 Int64.of_int]

let test_alloca_struct_array () =
  let f =
    Run.run
      Run.(unit @-> returning bool)
      ( local Stack.(arr Int64_pair.t (I64.v 2L)) @@ fun arr ->
        local Stack.(strct Int64_pair.r) @@ fun tmp ->
        end_frame @@ fun _self _ ->
        let* s1 = arr.%&[I64.v 0L] in
        let* s2 = arr.%&[I64.v 1L] in

        let* _ = s1.%{Int64_pair.f1} <- I64.v 1L in
        let* _ = s1.%{Int64_pair.f2} <- I64.v 2L in

        let* _ = s2.%{Int64_pair.f1} <- I64.v 3L in
        let* _ = s2.%{Int64_pair.f2} <- I64.v 4L in

        let* _ = tmp.%{Int64_pair.f1} <- s1.%{Int64_pair.f1} in
        let* _ = tmp.%{Int64_pair.f2} <- s1.%{Int64_pair.f2} in

        let* _ = arr.%[I64.v 0L] <- ~*s2 in
        let* _ = arr.%[I64.v 1L] <- ~*tmp in

        let* w = s1.%{Int64_pair.f1} in
        let* x = s1.%{Int64_pair.f2} in
        let* y = s2.%{Int64_pair.f1} in
        let* z = s2.%{Int64_pair.f2} in

        I64.(
          eq w (I64.v 3L)
          &&& eq x (I64.v 4L)
          &&& eq y (I64.v 1L)
          &&& eq z (I64.v 2L)) )
  in
  Alcotest.(check bool) "alloca_struct_array" true @@ f ()

let test_alloca_struct_cst_array () =
  let f =
    Run.run
      Run.(unit @-> returning bool)
      ( local Stack.(arr_cst Int64_pair.t 2L) @@ fun arr ->
        local Stack.(strct Int64_pair.r) @@ fun tmp ->
        end_frame @@ fun _self _ ->
        let* s1 = arr.%&[I64.v 0L] in
        let* s2 = arr.%&[I64.v 1L] in
        let* _ = s1.%{Int64_pair.f1} <- I64.v 1L in
        let* _ = s1.%{Int64_pair.f2} <- I64.v 2L in

        let* _ = s2.%{Int64_pair.f1} <- I64.v 3L in
        let* _ = s2.%{Int64_pair.f2} <- I64.v 4L in

        let* _ = tmp.%{Int64_pair.f1} <- s1.%{Int64_pair.f1} in
        let* _ = tmp.%{Int64_pair.f2} <- s1.%{Int64_pair.f2} in

        let* _ = arr.%[I64.v 0L] <- ~*s2 in
        let* _ = arr.%[I64.v 1L] <- ~*tmp in

        let* w = s1.%{Int64_pair.f1} in
        let* x = s1.%{Int64_pair.f2} in
        let* y = s2.%{Int64_pair.f1} in
        let* z = s2.%{Int64_pair.f2} in

        I64.(
          eq w (I64.v 3L)
          &&& eq x (I64.v 4L)
          &&& eq y (I64.v 1L)
          &&& eq z (I64.v 2L)) )
  in
  Alcotest.(check bool) "alloca_struct_cst_array" true @@ f ()

let test_alloca_fixed_size_array () =
  let f =
    Run.run
      Run.(unit @-> returning bool)
      ( local Stack.(arr Types.(arr_cst i64 3L) (I64.v 2L)) @@ fun arr ->
        local Stack.(num I64_num) @@ fun acc ->
        end_frame @@ fun _self _ ->
        let* arr0 = arr.%[I64.zero] in
        let* arr1 = arr.%[I64.one] in
        let* _ = for_loop 0L 2L (fun i -> arr0.%[i] <- i) in
        let* _ = for_loop 0L 2L (fun i -> arr1.%[i] <- i) in
        let* _ = acc <-- I64.zero in
        let* _ =
          for_loop 0L 2L (fun i ->
              for_loop 0L 1L (fun j -> acc <-- I64.add ~*acc arr.%[j].%[i]))
        in
        I64.eq ~*acc (I64.v 6L) )
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
  let main =
    let* bintree_sum =
      fundecl
        "bintree_sum"
        Types.(ptr Bintree.t @-> returning i64)
        ( end_frame @@ fun self node ->
          if_
            (is_null node)
            I64.zero
            (let* a = node.%{Bintree.a} in
             let* na = a.%[I64.zero] in
             let* nb = a.%[I64.one] in
             let* va = call1 self na in
             let* vb = call1 self nb in
             I64.add (I64.add node.%{Bintree.i} va) vb) )
    in
    fundecl
      "test_bintree"
      Types.(unit @-> returning bool)
      ( local Stack.(strct Bintree.r) @@ fun n1 ->
        local Stack.(strct Bintree.r) @@ fun n2 ->
        local Stack.(strct Bintree.r) @@ fun n3 ->
        end_frame @@ fun _self _unit ->
        let* _ = n1.%{Bintree.i} <- I64.v 41L in
        let* _ = n2.%{Bintree.i} <- I64.v 42L in
        let* _ = n3.%{Bintree.i} <- I64.v 43L in

        let* a1 = n1.%{Bintree.a} in
        let* _ = a1.%[I64.zero] <- n2 in
        let* _ = a1.%[I64.one] <- n3 in
        let* a2 = n2.%{Bintree.a} in
        let* _ = a2.%[I64.v 0L] <- null_ptr Bintree.t in
        let* _ = a2.%[I64.v 1L] <- null_ptr Bintree.t in
        let* a3 = n3.%{Bintree.a} in
        let* _ = a3.%[I64.v 0L] <- null_ptr Bintree.t in
        let* _ = a3.%[I64.v 1L] <- null_ptr Bintree.t in
        let* total = call1 bintree_sum n1 in
        I64.eq total (I64.v 126L) )
  in
  let f = Run.(run main (unit @-> returning bool)) in
  Alcotest.(check bool) "test_bintree" true @@ f ()

let test_set_cst_array_in_struct () =
  let f =
    Exec.run
      Exec.(unit @-> returning bool)
      ( local Stack.(strct Bintree.r) @@ fun strct ->
        local Stack.(arr_cst (Types.ptr Bintree.t) 2L) @@ fun arr ->
        end_frame @@ fun _self _ ->
        let* a = strct.%{Bintree.a} in
        let* _ = set a (I64.v 0L) strct in
        let* _ = set a (I64.v 1L) strct in
        let* _ = set arr (I64.v 0L) (null_ptr Bintree.t) in
        let* _ = set arr (I64.v 1L) (null_ptr Bintree.t) in
        let* _ = strct.%{Bintree.a} <- arr in
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
        let* a = bintree.%{Bintree.a} in
        set a (I64.v 0L) (null_ptr Bintree.t) >> fun () ->
        set a (I64.v 1L) (null_ptr Bintree.t) >> fun () ->
        (dummy.%{Dummy.dummy} <- load bintree) >> fun () ->
        let* inner = dummy.&{Dummy.dummy} in
        let* a = inner.%{Bintree.a} in
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
        let* a = bintree.%{Bintree.a} in
        set a (I64.v 0L) (null_ptr Bintree.t) >> fun () ->
        set a (I64.v 1L) (null_ptr Bintree.t) >> fun () ->
        (dummy.%{Dummy.dummy} <- bintree) >> fun () ->
        let* inner = dummy.%{Dummy.dummy} in
        let* a = inner.%{Bintree.a} in
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
        let* _ = arr.%[I64.v 0L] <- strct in
        let* _ = store ptrptr arr.%[I64.v 0L] in
        let* ptr = load ptrptr in
        let* _ = store ptr (load strct) in
        let* _ = strct.%{Int64_pair.f1} <- I64.v 42L in
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
            let* _ = arr.%[I64.v 0L] <- I64.v 42L in
            let* _ = arr.%[I64.v 1L] <- I64.v 43L in
            (* Store the address of [arr] in [ptr_arr] *)
            let* _ = ptr_arr.&[I64.v 0L] <- arr in
            (* Store the contents of [ptr_arr] in [ptr_ptr] *)
            let* _ = store ptrptr ptr_arr.%[I64.v 0L] in
            let* arr' = load (load ptrptr) in
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
       let* _ = strct.%{Bintree.i} <- I64.v 33L in
       let* _ = arr.%[I64.v 0L] <- strct in
       let* _ = arr'.%[I64.v 0L] <- strct in
       let* s' = arr.%[I64.v 0L] in
       let* s'' = arr'.%[I64.v 0L] in
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
       let* _ = arr.%[I64.v 0L] <- I64.v 1L in
       let* _ = arr.%[I64.v 1L] <- I64.v 2L in
       let* _ = arr_arr.&[I64.v 0L] <- arr in
       let* a = load @@ arr_arr.%[I64.v 0L] in
       I64.eq a.%[I64.v 0L] (I64.v 1L) && I64.eq a.%[I64.v 1L] (I64.v 2L))
  in
  Alcotest.(check bool) "set_cst_array_in_array" true (f ())

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
