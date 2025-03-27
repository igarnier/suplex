open Suplex
open Bigarray

(* Suplex implementation *)
let matmul =
  Run.jit
    ~cfg:Llvm_executionengine.{ default_compiler_options with opt_level = 3 }
    Run.(
      (* Input: (data, rows, cols) *)
      bigarray_f32 @-> i64 @-> i64 @-> returning (bigarray_f32 @-> i64 @-> i64)
      @-> bigarray_f32 @-> i64 @-> i64 @-> returning (bigarray_f32 @-> i64 @-> i64)
    )
    (end_frame @@ fun _self a_data a_rows a_cols b_data b_rows b_cols ->
      let* res_rows = a_rows in
      let* res_cols = b_cols in 
      let* inner = a_cols in
      
      (* Allocate result matrix *)
      let* res = malloc_array Types.f32 (I64.mul res_rows res_cols) in
      
      (* Initialize result to zero *)
      let* _ = 
        for_ 
          ~init:I64.zero 
          ~pred:(fun i -> I64.lt i (I64.mul res_rows res_cols))
          ~step:(fun i -> I64.add i I64.one)
          (fun i -> res.%[i] <-- F32.zero)
      in
      
      (* Matrix multiplication *)
      let* _ =
        for_ 
          ~init:I64.zero
          ~pred:(fun i -> I64.lt i res_rows)
          ~step:(fun i -> I64.add i I64.one)
          (fun i ->
            let* _ = 
              for_
                ~init:I64.zero
                ~pred:(fun k -> I64.lt k inner)
                ~step:(fun k -> I64.add k I64.one)
                (fun k ->
                  let* a_val = a_data.%[I64.add (I64.mul i inner) k] in
                  let* _ =
                    for_
                      ~init:I64.zero
                      ~pred:(fun j -> I64.lt j res_cols)
                      ~step:(fun j -> I64.add j I64.one)
                      (fun j ->
                        let* b_val = b_data.%[I64.add (I64.mul k res_cols) j] in
                        let* res_idx = I64.add (I64.mul i res_cols) j in
                        let* current = res.%[res_idx] in
                        res.%[res_idx] <-- F32.add current (F32.mul a_val b_val)
                      )
                  in
                  unit
                )
            in
            unit
          )
      in
      returning res res_rows res_cols
    )

(* Test helper *)
let test_matmul () =
  let a = Array1.of_array float32 c_layout [|1.; 2.; 3.; 4.|] in
  let b = Array1.of_array float32 c_layout [|5.; 6.; 7.; 8.|] in
  let (res, rows, cols) = matmul (a, 2L, 2L) (b, 2L, 2L) in
  Printf.printf "Result matrix (%Ldx%Ld):\n" rows cols;
  for i = 0 to Int64.to_int rows - 1 do
    for j = 0 to Int64.to_int cols - 1 do
      Printf.printf "%.1f " res.{i * Int64.to_int cols + j}
    done;
    print_newline ()
  done

(* OCaml reference implementation *)
let ocaml_matmul (a_data, a_rows, a_cols) (b_data, b_rows, b_cols) =
  let res_rows = Int64.to_int a_rows in
  let res_cols = Int64.to_int b_cols in
  let inner = Int64.to_int a_cols in
  let res = Array1.create float32 c_layout (res_rows * res_cols) in
  
  for i = 0 to res_rows - 1 do
    for k = 0 to inner - 1 do
      let a_val = a_data.{i * inner + k} in
      for j = 0 to res_cols - 1 do
        let b_val = b_data.{k * res_cols + j} in
        res.{i * res_cols + j} <- res.{i * res_cols + j} +. a_val *. b_val
      done
    done
  done;
  
  (res, a_rows, b_cols)

(* Benchmarking function *)
let benchmark () =
  let size = 64 in  (* Test with 64x64 matrices *)
  let data = Array.init (size * size) (fun i -> float_of_int (i + 1) /. 100.0) in
  let mat = 
    (Array1.of_array float32 c_layout data, 
     Int64.of_int size, 
     Int64.of_int size)
  in
  
  let time fn =
    let start = Unix.gettimeofday () in
    let _ = fn mat mat in
    Unix.gettimeofday () -. start
  in
  
  let ocaml_time = time ocaml_matmul in
  let suplex_time = time matmul in
  
  Printf.printf "OCaml implementation: %f seconds\n" ocaml_time;
  Printf.printf "Suplex implementation: %f seconds\n" suplex_time;
  Printf.printf "Speedup: %.2fx\n" (ocaml_time /. suplex_time)

let () = 
  test_matmul ();
  print_newline ();
  benchmark ()
