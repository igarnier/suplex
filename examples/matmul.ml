open Suplex
open Bigarray

type ba32 = (float, float32_elt, c_layout) Array1.t

type matrix = { ba : ba32; cols : int64; rows : int64 }

let pp_matrix fmt matrix =
  let open Format in
  let { ba; cols; rows } = matrix in
  fprintf fmt "@[<v>" ;
  for i = 0 to Int64.to_int rows - 1 do
    fprintf fmt "@[<h>" ;
    for j = 0 to Int64.to_int cols - 1 do
      fprintf fmt "%f " ba.{(i * Int64.to_int cols) + j}
    done ;
    fprintf fmt "@]@,"
  done ;
  fprintf fmt "@]"

let succ = I64.add I64.one

let exec_for_each start count f =
  block (List.init count (fun i -> f (I64.add start (I64.v (Int64.of_int i)))))

let unrolled_foldi count start acc f =
  let rec loop i acc =
    if i = count then acc else loop (Int64.succ i) (f I64.(add start (v i)) acc)
  in
  loop 0L acc

(* Suplex implementation *)
let matmul (type c r i) (cblsz : c Size.t) (rblsz : r Size.t) (iblsz : i Size.t)
    =
  let (module V) = make_f32_vec cblsz in
  let jitted_impl =
    Run.jit
      ~cfg:Llvm_executionengine.{ default_compiler_options with opt_level = 3 }
      Run.(
        bigarray_f32 @-> i64 @-> i64 @-> bigarray_f32 @-> i64 @-> i64
        @-> bigarray_f32 @-> returning unit)
      ( end_frame
      @@ fun _self a_data a_rows a_cols b_data _b_rows b_cols out_data ->
        let open I64.Infix in
        let* a_data = a_data.%{F32_ba.data} in
        let* b_data = b_data.%{F32_ba.data} in
        let* out_data = out_data.%{F32_ba.data} in

        let* b_data_as_vec = cast b_data (Types.num V.n) in
        let* out_data_as_vec = cast out_data (Types.num V.n) in
        let* iblsz' = v (Size.to_int64 iblsz) in
        let* cblsz' = v (Size.to_int64 cblsz) in
        let* rblsz' = v (Size.to_int64 rblsz) in

        (* let* i_blocks = a_cols / iblsz' in *)
        let* c_blocks = b_cols / cblsz' in
        (* let* r_blocks = a_rows / rblsz' in *)
        let get_a_elt row col = a_data.%[col + (row * a_cols)] in
        let get_b_vec row col_block =
          b_data_as_vec.%[col_block + (row * c_blocks)]
        in
        let get_out_vec row col_block =
          out_data_as_vec.%[col_block + (row * c_blocks)]
        in
        let set_out_vec row col_block v =
          out_data_as_vec.%[col_block + (row * c_blocks)] <- v
        in
        let for_each_block ~sz start stop =
          for_ ~init:start ~pred:(fun i -> i < stop) ~step:(fun i -> i + sz)
        in
        for_each_block ~sz:iblsz' I64.zero a_cols (fun i ->
            for_each_block ~sz:rblsz' I64.zero a_rows (fun r ->
                for_each_block ~sz:cblsz' I64.zero b_cols (fun c ->
                    let* c_block = c / cblsz' in
                    exec_for_each r (Size.to_int rblsz) @@ fun rr ->
                    let* acc =
                      unrolled_foldi
                        (Size.to_int64 iblsz)
                        i
                        (get_out_vec rr c_block)
                        (fun ii acc ->
                          (* Load data from A *)
                          let* a0 = get_a_elt rr ii in
                          let* v0 = broadcast F32_num a0 cblsz in
                          let* u0 = get_b_vec ii c_block in
                          V.add acc (V.mul v0 u0))
                    in
                    set_out_vec rr c_block acc))) )
  in
  fun a b res ->
    assert (a.cols = b.rows) ;
    assert (a.rows = res.rows) ;
    assert (b.cols = res.cols) ;
    assert (Int64.rem b.cols (Size.to_int64 cblsz) = 0L) ;
    assert (Int64.rem a.rows (Size.to_int64 rblsz) = 0L) ;
    assert (Int64.rem a.cols (Size.to_int64 iblsz) = 0L) ;
    jitted_impl a.ba a.rows a.cols b.ba b.rows b.cols res.ba

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

type ex_size = E : _ Size.t -> ex_size

(* Benchmarking function *)
let benchmark () =
  let size = 256 in
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
    let dt = Unix.gettimeofday () -. start in
    (res, dt)
  in

  let (res_ocaml, ocaml_time) = time ocaml_matmul in
  let (res_suplex, suplex_time) = time (matmul Size._8 Size._8 Size._8) in

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
  (* Format.printf "%a@." pp_matrix res_ocaml ; *)
  (* Format.printf "%a@." pp_matrix res_suplex ; *)
  Printf.printf "Speedup: %.2fx\n" (ocaml_time /. suplex_time)

(* let () = benchmark () *)

let sizes =
  [ E Size._1;
    E Size._2;
    E Size._4;
    E Size._8;
    E Size._16;
    E Size._32;
    E Size._64;
    E Size._128 ]

let mat_dims = [| 8; 64; 128; 256; 1024 |]

let mat1 size =
  { rows = Int64.of_int size;
    cols = Int64.of_int size;
    ba =
      Array1.of_array
        Float32
        C_layout
        (Array.init (size * size) (fun i -> float_of_int i /. 100.0))
  }

let mat2 size =
  { rows = Int64.of_int size;
    cols = Int64.of_int size;
    ba =
      Array1.of_array
        Float32
        C_layout
        (Array.init (size * size) (fun i -> float_of_int (i + 1) /. 100.0))
  }

let res size =
  { rows = Int64.of_int size;
    cols = Int64.of_int size;
    ba =
      Array1.of_array
        Float32
        C_layout
        (Array.init (size * size) (fun _i -> 0.0))
  }

let min f l =
  let with_v = List.map (fun x -> (x, f x)) l in
  List.sort (fun (_, v1) (_, v2) -> Float.compare v1 v2) with_v
  |> List.hd |> fst

let matmul =
  let memoized_impls = Hashtbl.create 11 in
  fun (E sc as c) (E sr as r) (E si as i) ->
    match Hashtbl.find_opt memoized_impls (c, r, i) with
    | None ->
        let impl = matmul sc sr si in
        Hashtbl.add memoized_impls (c, r, i) impl ;
        impl
    | Some impl -> impl

let results =
  let results = ref [] in
  Array.iter
    begin
      fun mat_dim ->
        Format.printf "mat_dim: %d\n%!" mat_dim ;
        let a = mat1 mat_dim in
        let b = mat2 mat_dim in
        let res = res mat_dim in
        let acc = ref [] in
        ListLabels.iter sizes ~f:(fun (E cblsz as c) ->
            ListLabels.iter sizes ~f:(fun (E rblsz as r) ->
                ListLabels.iter sizes ~f:(fun (E iblsz as i) ->
                    let icblsz = Size.to_int cblsz in
                    let irblsz = Size.to_int rblsz in
                    let iiblsz = Size.to_int iblsz in
                    Format.printf "%d %d %d\r%!" icblsz irblsz iiblsz ;
                    if
                      icblsz <= mat_dim && irblsz <= mat_dim
                      && iiblsz <= mat_dim
                      && irblsz * iiblsz <= 32 * 128
                    then begin
                      let jit_start = Unix.gettimeofday () in
                      let impl = matmul c r i in
                      let jit_stop = Unix.gettimeofday () in
                      Format.printf
                        "jitting %d %d %d %f\n%!"
                        icblsz
                        irblsz
                        iiblsz
                        (jit_stop -. jit_start) ;
                      let start = Unix.gettimeofday () in
                      impl a b res ;
                      let stop = Unix.gettimeofday () in
                      acc :=
                        (mat_dim, icblsz, irblsz, iiblsz, stop -. start) :: !acc
                    end))) ;
        let best = min (fun (_, _, _, _, dt) -> dt) !acc in
        results := best :: !results
    end
    mat_dims ;
  !results

let () =
  Out_channel.with_open_text "results.txt" @@ fun oc ->
  let fmtr = Format.formatter_of_out_channel oc in
  Format.fprintf fmtr "dim, cbl, rbl, ibl, t\n" ;
  List.iter
    (fun (mat_dim, c, r, i, dt) ->
      Format.fprintf fmtr "%d, %d, %d, %d, %f\n" mat_dim c r i dt)
    results
