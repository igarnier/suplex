open Type_system
open Intf

module Stack_frame (E : sig
  type (_, _, _, _) record

  type !'a typ

  type !'a m

  type !'a ptr

  type (!'a, 'c) arr
end) :
  Stack_frame
    with type ('a, 'b, 'c, 'd) record := ('a, 'b, 'c, 'd) E.record
     and type !'a typ := 'a E.typ
     and type 'a m := 'a E.m
     and type 'a ptr := 'a E.ptr
     and type ('a, 'c) arr := ('a, 'c) E.arr = struct
  open E

  type 'a stack_var =
    | SV_unit : unit ptr stack_var
    | SV_bool : bool ptr stack_var
    | SV_num : 'a numerical -> 'a ptr stack_var
    | SV_ptr : 'a typ -> 'a ptr ptr stack_var
    | SV_arr : 'a typ * i64 m -> ('a, [ `unk ]) arr stack_var
    | SV_arr_cst : 'a typ * int64 -> ('a, [ `cst ]) arr stack_var
    | SV_strct : (_, 'd vec, 'd vec, 't) record -> 't ptr stack_var

  type (_, _) t =
    | Empty : ('b, 'b) t
    | Cons : 'a stack_var * ('c, 'b) t -> ('a m -> 'c, 'b) t

  let empty = Empty

  let ( @+ ) v f = Cons (v, f)

  let unit = SV_unit

  let bool = SV_bool

  let num n = SV_num n

  let ptr ty = SV_ptr ty

  let arr ty len = SV_arr (ty, len)

  let arr_cst ty len =
    if len < 0L then invalid_arg "arr_cst: negative size" ;
    SV_arr_cst (ty, len)

  let strct r = SV_strct r
end
