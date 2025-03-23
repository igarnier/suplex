open Suplex
open Bigarray

let create_bigarray_f32 arr =
  let open Bigarray in
  Array1.of_array float32 c_layout arr

let dot_product_ocaml (arr1 : (float, float32_elt, c_layout) Array1.t)
    (arr2 : (float, float32_elt, c_layout) Array1.t) : float =
  let size1 = Array1.dim arr1 in
  let size2 = Array1.dim arr2 in
  if size1 <> size2 then failwith "Arrays must have the same size"
  else
    let sum = ref 0.0 in
    for i = 0 to size1 - 1 do
      sum := !sum +. (arr1.{i} *. arr2.{i})
    done ;
    !sum

let dot_product_vanilla =
  Run.jit
    ~cfg:Llvm_executionengine.{ default_compiler_options with opt_level = 3 }
    ~debug:true
    Run.(bigarray_f32 @-> bigarray_f32 @-> returning f32)
    ( end_frame @@ fun _self vec1 vec2 ->
      let* size1 = vec1.%{F32_ba.dim} in
      let* size2 = vec2.%{F32_ba.dim} in
      let* _ =
        if_ (I64.eq size1 size2) unit (fail "Arrays must have the same size")
      in
      let data1 = vec1.%{F32_ba.data} in
      let data2 = vec2.%{F32_ba.data} in
      foldi
        ~init:I64.zero
        ~acc:F32.zero
        ~pred:(fun i _ -> I64.lt i size1)
        ~step:(fun i -> I64.add i I64.one)
        (fun i acc ->
          let* v1 = data1.%[i] in
          let* v2 = data2.%[i] in
          F32.add acc (F32.mul v1 v2)) )

let dot_product_fma =
  let mdl =
    Run.add_intrinsic Suplex_intrinsics.F32.fma @@ fun fma ->
    Run.main
      "dot_product"
      Run.(bigarray_f32 @-> bigarray_f32 @-> returning f32)
      ( end_frame @@ fun _self vec1 vec2 ->
        let* size1 = vec1.%{F32_ba.dim} in
        let* size2 = vec2.%{F32_ba.dim} in
        let* _ =
          if_ (I64.eq size1 size2) unit (fail "Arrays must have the same size")
        in
        let data1 = vec1.%{F32_ba.data} in
        let data2 = vec2.%{F32_ba.data} in
        foldi
          ~init:I64.zero
          ~acc:F32.zero
          ~pred:(fun i _ -> I64.lt i size1)
          ~step:(fun i -> I64.add i I64.one)
          (fun i acc ->
            let* v1 = data1.%[i] in
            let* v2 = data2.%[i] in
            call3 fma acc v1 v2) )
  in
  Run.jit_module
    ~cfg:Llvm_executionengine.{ default_compiler_options with opt_level = 3 }
    ~debug:true
    mdl

let measure_time f x y =
  let start_time = Unix.gettimeofday () in
  let _ = f x y in
  let end_time = Unix.gettimeofday () in
  end_time -. start_time

(* Performance comparison *)
let () =
  let size = 1 lsl 20 in
  let arr1 =
    create_bigarray_f32 (Array.init size (fun i -> 0.00001 *. float_of_int i))
  in
  let arr2 =
    create_bigarray_f32
      (Array.init size (fun i -> (0.00001 *. float_of_int i) +. 1.0))
  in

  let time_bigarray = measure_time dot_product_ocaml arr1 arr2 in
  let time_suplex = measure_time dot_product_vanilla arr1 arr2 in
  let time_suplex_fma = measure_time dot_product_fma arr1 arr2 in

  Printf.printf "Bigarray implementation time: %f seconds\n" time_bigarray ;
  Printf.printf "Suplex implementation time: %f seconds\n" time_suplex ;
  Printf.printf "Suplex implementation time (fma): %f seconds\n" time_suplex_fma
