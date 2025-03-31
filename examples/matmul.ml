open Suplex
open Bigarray

type ba32 = (float, float32_elt, c_layout) Array1.t

type matrix = { ba : ba32; cols : int64; rows : int64 }

let succ = I64.add I64.one

let for_each start stop =
  for_ ~init:start ~pred:(fun i -> I64.lt i stop) ~step:succ

(* Suplex implementation *)
let matmul a b res =
  assert (a.cols = b.rows) ;
  assert (a.rows = res.rows) ;
  assert (b.cols = res.cols) ;

  Run.jit
    ~cfg:Llvm_executionengine.{ default_compiler_options with opt_level = 3 }
    Run.(
      bigarray_f32 @-> i64 @-> i64 @-> bigarray_f32 @-> i64 @-> i64
      @-> bigarray_f32 @-> returning unit)
    ( end_frame
    @@ fun _self a_data a_rows a_cols b_data _b_rows b_cols out_data ->
      let* inner = a_cols in

      let* a_data = a_data.%{F32_ba.data} in
      let* b_data = b_data.%{F32_ba.data} in
      let* res = out_data.%{F32_ba.data} in

      let get_a_elt row col = a_data.%[I64.(add col (mul row a_cols))] in
      let get_b_elt row col = b_data.%[I64.(add col (mul row b_cols))] in

      (* Initialize result to zero *)
      (* let* _ = *)
      (*   for_ *)
      (*     ~init:I64.zero *)
      (*     ~pred:(fun i -> I64.lt i (I64.mul res_rows res_cols)) *)
      (*     ~step:(fun i -> I64.add i I64.one) *)
      (*     (fun i -> res.%[i] <- F32.zero) *)
      (* in *)
      let* block_size = I64.v 8L in
      let _for_each_block start stop =
        for_
          ~init:start
          ~pred:(fun i -> I64.lt i stop)
          ~step:(I64.add block_size)
      in
      for_each I64.zero b_cols (fun i ->
          for_each I64.zero a_rows (fun j ->
              let* res_idx = I64.add (I64.mul j b_cols) i in
              let* acc = res.%[res_idx] in
              let* tot =
                foldi
                  ~init:I64.zero
                  ~acc
                  ~pred:(fun k _acc -> I64.lt k inner)
                  ~step:succ
                  (fun k acc ->
                    let* a_val = get_a_elt j k in
                    let* b_val = get_b_elt k i in
                    F32.add acc (F32.mul a_val b_val))
              in
              res.%[res_idx] <- tot)) )
    a.ba
    a.rows
    a.cols
    b.ba
    b.rows
    b.cols
    res.ba

(* OCaml reference implementation *)
let ocaml_matmul a b res =
  assert (a.cols = b.rows) ;
  assert (a.rows = res.rows) ;
  assert (b.cols = res.cols) ;
  let res_rows = Int64.to_int res.rows in
  let res_cols = Int64.to_int res.cols in
  let inner = Int64.to_int a.cols in
  let a_data = a.ba in
  let b_data = b.ba in
  let r_data = res.ba in

  for i = 0 to res_rows - 1 do
    for k = 0 to inner - 1 do
      let a_val = a_data.{(i * inner) + k} in
      for j = 0 to res_cols - 1 do
        let b_val = b_data.{(k * res_cols) + j} in
        r_data.{(i * res_cols) + j} <-
          r_data.{(i * res_cols) + j} +. (a_val *. b_val)
      done
    done
  done

(* Benchmarking function *)
let benchmark () =
  let size = 128 in
  (* Test with 64x64 matrices *)
  let mat1 =
    { rows = Int64.of_int size;
      cols = Int64.of_int size;
      ba =
        Array1.of_array
          Float32
          C_layout
          (Array.init (size * size) (fun i -> float_of_int i /. 100.0))
    }
  in
  let mat2 =
    { rows = Int64.of_int size;
      cols = Int64.of_int size;
      ba =
        Array1.of_array
          Float32
          C_layout
          (Array.init (size * size) (fun i -> float_of_int (i + 1) /. 100.0))
    }
  in
  let res () =
    { rows = Int64.of_int size;
      cols = Int64.of_int size;
      ba =
        Array1.of_array
          Float32
          C_layout
          (Array.init (size * size) (fun _i -> 0.0))
    }
  in

  let time fn =
    let res = res () in
    let start = Unix.gettimeofday () in
    let () = fn mat1 mat2 res in
    (res, Unix.gettimeofday () -. start)
  in

  let (res_ocaml, ocaml_time) = time ocaml_matmul in
  let (res_suplex, suplex_time) = time matmul in

  let l1_dist mat1 mat2 =
    let acc = ref 0.0 in
    for i = 0 to Array1.dim mat1.ba - 1 do
      let v1 = mat1.ba.{i} in
      let v2 = mat2.ba.{i} in
      let v = v1 -. v2 in
      acc := !acc +. abs_float v
    done ;
    !acc
  in

  Printf.printf "OCaml implementation: %f seconds\n" ocaml_time ;
  Printf.printf "Suplex implementation: %f seconds\n" suplex_time ;
  Printf.printf "Dist: %.2f\n" (l1_dist res_ocaml res_suplex) ;
  Printf.printf "Speedup: %.2fx\n" (ocaml_time /. suplex_time)

let () = benchmark ()
