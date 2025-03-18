open Suplex

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

  type 'sz mask = (bool, 'sz) vec expr

  let mask_type sz = vector I1_num sz

  let vp_select scty len =
    intrinsic
      (Format.asprintf
         "llvm.vp.select.v%d%a"
         (Size.to_int len)
         Types.pp_base_numerical
         scty)
      (mask_type len @-> vector scty len @-> vector scty len @-> scalar I32_num
      @-> returning (vector scty len))

  let vp_merge scty len =
    intrinsic
      (Format.asprintf
         "llvm.vp.merge.v%d%a"
         (Size.to_int len)
         Types.pp_base_numerical
         scty)
      (mask_type len @-> vector scty len @-> vector scty len
      @-> returning (vector scty len))

  let vp_unop op scty len =
    intrinsic
      (Format.asprintf
         "llvm.vp.%s.v%d%a"
         op
         (Size.to_int len)
         Types.pp_base_numerical
         scty)
      (vector scty len @-> scalar I1_num @-> mask_type len @-> scalar I32_num
      @-> returning (vector scty len))

  let vp_binop op scty len =
    intrinsic
      (Format.asprintf
         "llvm.vp.%s.v%d%a"
         op
         (Size.to_int len)
         Types.pp_base_numerical
         scty)
      (vector scty len @-> vector scty len @-> mask_type len @-> scalar I32_num
      @-> returning (vector scty len))
end
