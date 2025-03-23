open Suplex

module type Float_intrinsics = sig
  type t

  val sqrt : (t expr -> t expr) intrinsic

  val powi : (t expr -> i32 expr -> t expr) intrinsic

  val sin : (t expr -> t expr) intrinsic

  val cos : (t expr -> t expr) intrinsic

  val tan : (t expr -> t expr) intrinsic

  val asin : (t expr -> t expr) intrinsic

  val acos : (t expr -> t expr) intrinsic

  val atan : (t expr -> t expr) intrinsic

  val atan2 : (t expr -> t expr -> t expr) intrinsic

  val sinh : (t expr -> t expr) intrinsic

  val cosh : (t expr -> t expr) intrinsic

  val tanh : (t expr -> t expr) intrinsic

  val pow : (t expr -> t expr -> t expr) intrinsic

  val exp : (t expr -> t expr) intrinsic

  val exp2 : (t expr -> t expr) intrinsic

  val exp10 : (t expr -> t expr) intrinsic

  val log : (t expr -> t expr) intrinsic

  val log10 : (t expr -> t expr) intrinsic

  val fma : (t expr -> t expr -> t expr -> t expr) intrinsic

  val log2 : (t expr -> t expr) intrinsic
end

module Enrich (Base : sig
  type s

  val num : s base_numerical
end) =
struct
  let sf = Printf.sprintf

  let ty = Types.base_num Base.num

  let intrinsic n ty =
    intrinsic
      ("llvm." ^ n (Format.asprintf "%a" Types.pp_base_numerical Base.num))
      ty

  let sqrt = intrinsic (sf "sqrt.%s") (ty @-> returning ty)

  let powi =
    intrinsic (sf "powi.%s.i32") (ty @-> scalar I32_num @-> returning ty)

  let sin = intrinsic (sf "sin.%s") (ty @-> returning ty)

  let cos = intrinsic (sf "cos.%s") (ty @-> returning ty)

  let tan = intrinsic (sf "tan.%s") (ty @-> returning ty)

  let asin = intrinsic (sf "asin.%s") (ty @-> returning ty)

  let acos = intrinsic (sf "acos.%s") (ty @-> returning ty)

  let atan = intrinsic (sf "atan.%s") (ty @-> returning ty)

  let atan2 = intrinsic (sf "atan.%s") (ty @-> ty @-> returning ty)

  let sinh = intrinsic (sf "sinh.%s") (ty @-> returning ty)

  let cosh = intrinsic (sf "cosh.%s") (ty @-> returning ty)

  let tanh = intrinsic (sf "tanh.%s") (ty @-> returning ty)

  let pow = intrinsic (sf "pow.%s") (ty @-> ty @-> returning ty)

  let exp = intrinsic (sf "exp.%s") (ty @-> returning ty)

  let exp2 = intrinsic (sf "exp2.%s") (ty @-> returning ty)

  let exp10 = intrinsic (sf "exp10.%s") (ty @-> returning ty)

  let log = intrinsic (sf "log.%s") (ty @-> returning ty)

  let log10 = intrinsic (sf "log10.%s") (ty @-> returning ty)

  let log2 = intrinsic (sf "log2.%s") (ty @-> returning ty)

  let fma = intrinsic (sf "fma.%s") (ty @-> ty @-> ty @-> returning ty)
end

module F64 : sig
  include module type of F64

  include Float_intrinsics with type t := f64
end = struct
  include Suplex.F64

  include Enrich (struct
    type s = f64

    let num = F64_num
  end)
end

module F32 : sig
  include module type of F32

  include Float_intrinsics with type t := f32
end = struct
  include Suplex.F32

  include Enrich (struct
    type s = f32

    let num = F32_num
  end)
end

module Vector = struct
  module Reduce = struct
    type op =
      | Add
      | Mul
      | FAdd
      | FMul
      | And
      | Or
      | Xor
      | SMax
      | SMin
      | UMax
      | UMin
      | FMax
      | FMin
      | FMaximum
      | FMinimum

    let add = Add

    let mul = Mul

    let fadd = FAdd

    let fmul = FMul

    let and_ = And

    let or_ = Or

    let xor = Xor

    let smax = SMax

    let smin = SMin

    let umax = UMax

    let umin = UMin

    let fmax = FMax

    let fmin = FMin

    let fmaximum = FMaximum

    let fminimum = FMinimum

    let string_of_op op =
      match op with
      | Add -> "add"
      | Mul -> "mul"
      | FAdd -> "fadd"
      | FMul -> "fmul"
      | And -> "and"
      | Or -> "or"
      | Xor -> "xor"
      | SMax -> "smax"
      | SMin -> "smin"
      | UMax -> "umax"
      | UMin -> "umin"
      | FMax -> "fmax"
      | FMin -> "fmin"
      | FMaximum -> "fmaximum"
      | FMinimum -> "fminimum"

    let assert_op_compatible_with_kind = function
      | ((Add | Mul | And | Or | Xor | SMax | SMin | UMax | UMin), `int) -> ()
      | ((FAdd | FMul | FMax | FMin | FMaximum | FMinimum), `fp) -> ()
      | (op, `int) ->
          Format.kasprintf
            failwith
            "Reduction %s not compatible with integer type"
            (string_of_op op)
      | (op, `fp) ->
          Format.kasprintf
            failwith
            "Reduction %s not compatible with floating-point type"
            (string_of_op op)

    let reduce (type s len) (op : op) (scty : s base_numerical)
        (len : len Size.t) =
      assert_op_compatible_with_kind (op, base_numerical_kind scty) ;
      let name = string_of_op op in
      intrinsic
        (Format.asprintf
           "llvm.vector.reduce.%s.v%d%a"
           name
           (Size.to_int len)
           Types.pp_base_numerical
           scty)
        (vector scty len @-> returning (scalar scty))

    let reduce_acc (type s len) (op : op) (scty : s base_numerical)
        (len : len Size.t) =
      (match op with
      | FAdd | FMul -> ()
      | _ -> failwith "reduce_acc: only supported for FAdd and FMul") ;
      assert_op_compatible_with_kind (op, base_numerical_kind scty) ;
      let name = string_of_op op in
      intrinsic
        (Format.asprintf
           "llvm.vector.reduce.%s.v%d%a"
           name
           (Size.to_int len)
           Types.pp_base_numerical
           scty)
        (scalar scty @-> vector scty len @-> returning (scalar scty))
  end

  (* type 'sz mask = (i1, 'sz) vec expr *)

  (* let mask_type sz = vector I1_num sz *)

  (* let vp_select scty len = *)
  (*   intrinsic *)
  (*     (Format.asprintf *)
  (*        "llvm.vp.select.v%d%a" *)
  (*        (Size.to_int len) *)
  (*        Types.pp_base_numerical *)
  (*        scty) *)
  (*     (mask_type len @-> vector scty len @-> vector scty len @-> scalar I32_num *)
  (*     @-> returning (vector scty len)) *)

  (* let vp_merge scty len = *)
  (*   intrinsic *)
  (*     (Format.asprintf *)
  (*        "llvm.vp.merge.v%d%a" *)
  (*        (Size.to_int len) *)
  (*        Types.pp_base_numerical *)
  (*        scty) *)
  (*     (mask_type len @-> vector scty len @-> vector scty len *)
  (*     @-> returning (vector scty len)) *)

  (* let vp_unop op scty len = *)
  (*   intrinsic *)
  (*     (Format.asprintf *)
  (*        "llvm.vp.%s.v%d%a" *)
  (*        op *)
  (*        (Size.to_int len) *)
  (*        Types.pp_base_numerical *)
  (*        scty) *)
  (*     (vector scty len @-> scalar I1_num @-> mask_type len @-> scalar I32_num *)
  (*     @-> returning (vector scty len)) *)

  (* let vp_binop op scty len = *)
  (*   intrinsic *)
  (*     (Format.asprintf *)
  (*        "llvm.vp.%s.v%d%a" *)
  (*        op *)
  (*        (Size.to_int len) *)
  (*        Types.pp_base_numerical *)
  (*        scty) *)
  (*     (vector scty len @-> vector scty len @-> mask_type len @-> scalar I32_num *)
  (*     @-> returning (vector scty len)) *)
end
