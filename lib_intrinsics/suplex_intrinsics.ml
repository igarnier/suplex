open Suplex

module Vector = struct
  type reduce_op =
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

  let reduce (type s len) (op : reduce_op) (scty : s base_numerical)
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

  let reduce_acc (type s len) (op : reduce_op) (scty : s base_numerical)
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
