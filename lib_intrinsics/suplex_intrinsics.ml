open Suplex

let suffix_of_vector_type : type a sz. (a, sz) vec typ -> string =
  fun (typ : (a, sz) vec typ) ->
  (match typ with
   | TNum (Base_num _) -> assert false
   | TRecord _ -> assert false
   | TFn _ -> assert false
   | TNum (Vec_num { base; numel }) ->
     let numel = Size.to_int numel in
     let base = Format.asprintf "%a" Types.pp_base_numerical base in
     Printf.sprintf "v%d%s" numel base
  )

let make_intrinsic base_name vec_typ proto =
  let suffix = suffix_of_vector_type vec_typ in
  let name = Printf.sprintf "%s.%s" base_name suffix in
  intrinsic name proto

module Vector = struct
  let reduce_add_v4i32 = make_intrinsic "llvm.vector.reduce.add" (vector i32_num Size._4) ((vector i32_num Size._4) @-> returning (scalar i32_num))
  let reduce_add_v2i64 = make_intrinsic "llvm.vector.reduce.add" (vector i64_num Size._2) ((vector i64_num Size._2) @-> returning (scalar i64_num))
end
