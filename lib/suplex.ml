module Vec = Vec
module Types = Types
module Syntax = Syntax
module Compile = Compile
module Size = Size
open Syntax

type i64 = Syntax.i64 = I64

type i32 = Syntax.i32 = I32

type i16 = Syntax.i16 = I16

type i8 = Syntax.i8 = I8

type i1 = Syntax.i1 = I1

type f32 = Syntax.f32 = F32

type f64 = Syntax.f64 = F64

type ('a, 'sz) vec = ('a, 'sz) Syntax.vec = Vector

type 'a base_numerical = 'a Syntax.base_numerical =
  | I64_num : i64 base_numerical
  | I32_num : i32 base_numerical
  | I16_num : i16 base_numerical
  | I8_num : i8 base_numerical
  | I1_num : i1 base_numerical
  | F64_num : f64 base_numerical
  | F32_num : f32 base_numerical

type 'a numerical = 'a Syntax.numerical =
  | Base_num : 'a base_numerical -> 'a numerical
  | Vec_num :
      { base : 'a base_numerical; numel : 'sz Size.t }
      -> ('a, 'sz) vec numerical

type 'a typ = 'a Syntax.typ =
  | TUnit : unit typ
  | TBool : bool typ
  | TNum : 'a numerical -> 'a typ
  | TPtr : 'a typ -> 'a ptr typ
  | TArr_unk : 'a typ -> ('a, [ `unk ]) arr typ
  | TArr_cst : 'a typ * int64 -> ('a, [ `cst ]) arr typ
  | TRecord :
      { descr : (_, 'u Vec.t, 'u Vec.t, 't record) record_desc }
      -> 't record typ
  | TFn : 'a fn -> 'a fn typ

type 'a ptr = 'a Syntax.ptr = Ptr

type 'a record = 'a Syntax.record = Record

type ('a, 'b, 'c, 'd) record_desc = ('a, 'b, 'c, 'd) Syntax.record_desc

type ('f, 'r) field = ('f, 'r) Syntax.field =
  | Field : { name : string; ty : 'a typ } -> ('a, 't) field

type ('a, 'k) arr = ('a, 'k) Syntax.arr = Arr

type 'a fn = 'a Syntax.fn

type 'a stack = 'a Syntax.stack

type 'a stack_var = 'a Syntax.stack_var

type 'a expr = 'a Syntax.expr

type 'a fundecl = 'a Syntax.fundecl

type 'a intrinsic = 'a Syntax.intrinsic

type ('a, 'b) args = ('a, 'b) Syntax.args

type ('s, 'o) num_rel = ('s, 'o) Syntax.num_rel

type 'sz mask = 'sz Syntax.mask

module type Numerical = sig
  (** [t] is the numerical type. *)
  type t

  (** [v] is the type of constant values. *)
  type v

  (** Witness that [t] is related to OCaml constants of type [v]. *)
  val rel : (t, v) Syntax.num_rel

  (** Witness that [t] is a numerical type. *)
  val n : t numerical

  (** [v c] is a constant equal to [c]. *)
  val v : v -> t expr

  val add : t expr -> t expr -> t expr

  val sub : t expr -> t expr -> t expr

  val mul : t expr -> t expr -> t expr

  val div : t expr -> t expr -> t expr

  val rem : t expr -> t expr -> t expr

  val neg : t expr -> t expr

  val lt : t expr -> t expr -> bool expr

  val le : t expr -> t expr -> bool expr

  val eq : t expr -> t expr -> bool expr

  val zero : t expr

  val one : t expr

  module Infix : sig
    val v : v -> t expr

    val ( + ) : t expr -> t expr -> t expr

    val ( - ) : t expr -> t expr -> t expr

    val ( * ) : t expr -> t expr -> t expr

    val ( / ) : t expr -> t expr -> t expr

    val ( mod ) : t expr -> t expr -> t expr

    val ( ~- ) : t expr -> t expr

    val ( < ) : t expr -> t expr -> bool expr

    val ( <= ) : t expr -> t expr -> bool expr

    val ( = ) : t expr -> t expr -> bool expr

    val zero : t expr

    val one : t expr
  end
end

let zero_of_base_num_rel : type t v. (t, v) base_num_rel -> v =
 fun rel ->
  match rel with
  | I64_rel -> 0L
  | I32_rel -> 0l
  | I16_rel -> 0
  | I8_rel -> 0
  | I1_rel -> false
  | F64_rel -> 0.0
  | F32_rel -> 0.0

let one_of_base_num_rel : type t v. (t, v) base_num_rel -> v =
 fun rel ->
  match rel with
  | I64_rel -> 1L
  | I32_rel -> 1l
  | I16_rel -> 1
  | I8_rel -> 1
  | I1_rel -> true
  | F64_rel -> 1.0
  | F32_rel -> 1.0

let zero_of_num_rel : type t v. (t, v) num_rel -> v =
 fun rel ->
  match rel with
  | Base_rel rel -> zero_of_base_num_rel rel
  | Vec_rel { base; numel } ->
      let zero = zero_of_base_num_rel base in
      Array.make (Size.to_int numel) zero

let one_of_num_rel : type t v. (t, v) num_rel -> v =
 fun rel ->
  match rel with
  | Base_rel rel -> one_of_base_num_rel rel
  | Vec_rel { base; numel } ->
      let one = one_of_base_num_rel base in
      Array.make (Size.to_int numel) one

module Make_numerical (Num : sig
  type t

  type v

  val rel : (t, v) num_rel
end) : Numerical with type t = Num.t and type v = Num.v = struct
  type t = Num.t

  type v = Num.v

  let rel = Num.rel

  let n = Compile.numerical_of_num_rel rel

  let v x = Syntax.Num (rel, x)

  let zero = v (zero_of_num_rel rel)

  let one = v (one_of_num_rel rel)

  let add a b = Add (n, a, b)

  let sub a b = Sub (n, a, b)

  let mul a b = Mul (n, a, b)

  let div a b = Div (n, a, b)

  let neg a = Neg (n, a)

  let lt a b = Lt (n, a, b)

  let le a b = Le (n, a, b)

  let eq a b = Eq (n, a, b)

  let rem a b = Rem (n, a, b)

  module Infix = struct
    let v = v

    let ( + ) = add

    let ( - ) = sub

    let ( * ) = mul

    let ( / ) = div

    let ( mod ) = rem

    let ( ~- ) = neg

    let ( < ) = lt

    let ( <= ) = le

    let ( = ) = eq

    let zero = zero

    let one = one
  end
end

(* Specialize [Make_numerical] to base numerical types. *)
module Make_base_numerical (Base : sig
  type t

  type v

  val rel : (t, v) base_num_rel
end) =
Make_numerical (struct
  type t = Base.t

  type v = Base.v

  let rel = Base_rel Base.rel
end)

module type Base_num_rel = sig
  type t

  type v

  val rel : (t, v) base_num_rel
end

let module_of_num_rel : type t v.
    (t, v) base_num_rel -> (module Base_num_rel with type t = t and type v = v)
    =
 fun rel ->
  (module struct
    type nonrec t = t

    type nonrec v = v

    let rel = rel
  end)

module I64 = Make_base_numerical ((val module_of_num_rel I64_rel))

module I32 = Make_base_numerical ((val module_of_num_rel I32_rel))

module I16 = Make_base_numerical ((val module_of_num_rel I16_rel))

module I8 = Make_base_numerical ((val module_of_num_rel I8_rel))

module I1 = Make_base_numerical ((val module_of_num_rel I1_rel))

module F64 = Make_base_numerical ((val module_of_num_rel F64_rel))

module F32 = Make_base_numerical ((val module_of_num_rel F32_rel))

module Make_vec (Rel : sig
  type t

  type v

  val rel : (t, v) base_num_rel
end) (Dim : sig
  type dim

  val dim : dim Size.t
end) =
Make_numerical (struct
  type t = (Rel.t, Dim.dim) vec

  type v = Rel.v array

  let rel = Vec_rel { base = Rel.rel; numel = Dim.dim }
end)

let make_vec_from_rel (type t v dim) (dim : dim Size.t)
    (rel : (t, v) base_num_rel) :
    (module Numerical with type t = (t, dim) vec and type v = v array) =
  let module R =
    Make_vec
      ((val module_of_num_rel rel))
      (struct
        type nonrec dim = dim

        let dim = dim
      end)
  in
  (module R)

let make_i64_vec dim = make_vec_from_rel dim I64_rel

let make_i32_vec dim = make_vec_from_rel dim I32_rel

let make_i16_vec dim = make_vec_from_rel dim I16_rel

let make_i8_vec dim = make_vec_from_rel dim I8_rel

let make_i1_vec dim = make_vec_from_rel dim I1_rel

let make_f64_vec dim = make_vec_from_rel dim F64_rel

let make_f32_vec dim = make_vec_from_rel dim F32_rel

module type BA = Run.BA

module I64_ba = Run.I64_ba
module I32_ba = Run.I32_ba
module I16_ba = Run.I16_ba
module I8_ba = Run.I8_ba
module F64_ba = Run.F64_ba
module F32_ba = Run.F32_ba

let scalar s = TNum (Base_num s)

let vector base numel = TNum (Vec_num { base; numel })

let base_numerical_kind = base_numerical_kind

let i64_num = I64_num

let i32_num = I32_num

let i16_num = I16_num

let i8_num = I8_num

let f64_num = F64_num

let f32_num = F32_num

let returning = Types.returning

let ( @-> ) dom range = Types.(dom @-> range)

let ( let* ) m f = Let (m, f)

let unit = Unit

let tt = True

let ff = False

let const_array (type t v) (module N : Numerical with type t = t and type v = v)
    arr =
  Const_array (N.rel, arr)

let string ?(strz = false) str = String { strz; str }

let ( &&& ) a b = And (a, b)

let ( ||| ) a b = Or (a, b)

let ( <-- ) a b = Store (a, b)

let ( ~* ) a = Load a

let is_null a = IsNull a

let null_ptr ty = NullPtr ty

let ( .%[] ) a b = Get (a, b)

let ( .%&[] ) a b = GetAddr (a, b)

let ( .%[]<- ) a b c = Set (a, b, c)

let ( .%{} ) a b = GetField (b, a)

let ( .%&{} ) a b = GetFieldAddr (b, a)

let ( .%{}<- ) a b c = SetField (b, a, c)

let cast ar ty = Cast (ar, ty)

let if_ c ift iff = Cond (c, ift, iff)

let for_ ~init ~pred ~step body = For { init; pred; step; body }

let foldi ~init ~acc ~pred ~step body = Foldi { init; acc; pred; step; body }

let while_ pred body = While (pred, body)

let switch e ~default cases = Switch { e; cases; default }

let call f args = Call (f, args)

let call1 f arg = Call (f, Args_cons (arg, Args_nil))

let call2 f arg1 arg2 = Call (f, Args_cons (arg1, Args_cons (arg2, Args_nil)))

let call3 f arg1 arg2 arg3 =
  Call (f, Args_cons (arg1, Args_cons (arg2, Args_cons (arg3, Args_nil))))

let empty_args = Args_nil

let arg a rest = Args_cons (a, rest)

let addr_of_arr arr = AddrOf (Addressable_array, arr)

let addr_of_rec arr = AddrOf (Addressable_record, arr)

let ptr_eq a b = PtrEq (a, b)

let fail msg = Fail msg

let malloc ty = Malloc ty

let malloc_array ty len = Malloc_array (ty, len)

let free ptr = Free ptr

let free_array arr = Free_array arr

let base_can_be_truncated : type a b.
    a base_numerical -> b base_numerical -> (unit, string) Result.t =
 fun n1 n2 ->
  match (n1, n2) with
  | (I64_num, I32_num)
  | (I64_num, I16_num)
  | (I64_num, I8_num)
  | (I32_num, I16_num)
  | (I32_num, I8_num)
  | (I16_num, I8_num) ->
      Ok ()
  | _ ->
      Format.kasprintf
        Result.error
        "Cannot truncate %a to %a"
        Types.pp_base_numerical
        n1
        Types.pp_base_numerical
        n2

let can_be_truncated : type a b.
    a numerical -> b numerical -> (unit, string) Result.t =
 fun n1 n2 ->
  match (n1, n2) with
  | (Base_num n1, Base_num n2) -> base_can_be_truncated n1 n2
  | (Vec_num { base = n1; _ }, Vec_num { base = n2; _ }) ->
      base_can_be_truncated n1 n2
  | _ ->
      Format.kasprintf
        Result.error
        "Cannot truncate %a to %a"
        Types.pp_numerical
        n1
        Types.pp_numerical
        n2

let trunc src dst v =
  match can_be_truncated src dst with
  | Error e -> failwith e
  | Ok () -> Trunc (src, dst, v)

let base_can_be_extended : type a b.
    a base_numerical -> b base_numerical -> (unit, string) Result.t =
 fun n1 n2 ->
  match (n1, n2) with
  | (I8_num, I16_num)
  | (I8_num, I32_num)
  | (I8_num, I64_num)
  | (I16_num, I32_num)
  | (I16_num, I64_num)
  | (I32_num, I64_num) ->
      Ok ()
  | _ ->
      Format.kasprintf
        Result.error
        "Cannot extend %a to %a"
        Types.pp_base_numerical
        n1
        Types.pp_base_numerical
        n2

let can_be_extended : type a b.
    a numerical -> b numerical -> (unit, string) Result.t =
 fun n1 n2 ->
  match (n1, n2) with
  | (Base_num n1, Base_num n2) -> base_can_be_extended n1 n2
  | (Vec_num { base = n1; _ }, Vec_num { base = n2; _ }) ->
      base_can_be_extended n1 n2
  | _ ->
      Format.kasprintf
        Result.error
        "Cannot extend %a to %a"
        Types.pp_numerical
        n1
        Types.pp_numerical
        n2

let sext src dst v =
  match can_be_extended src dst with
  | Error e -> failwith e
  | Ok () -> SExt (src, dst, v)

let zext src dst v =
  match can_be_extended src dst with
  | Error e -> failwith e
  | Ok () -> ZExt (src, dst, v)

let to_f32 src v = ToF32 (src, v)

let to_f64 src v = ToF64 (src, v)

let of_f32 trgt v = OfF32 (trgt, v)

let of_f64 trgt v = OfF64 (trgt, v)

let vec_to_f32 src v = VecToF32 (src, v)

let vec_to_f64 src v = VecToF64 (src, v)

let vec_of_f32 trgt v = VecOfF32 (trgt, v)

let vec_of_f64 trgt v = VecOfF64 (trgt, v)

let mask size mask =
  if Array.length mask <> Size.to_int size then
    Format.kasprintf
      invalid_arg
      "mask: prescribed size %d does not match array length %d"
      (Size.to_int size)
      (Array.length mask) ;
  { size; mask }

let shuffle lhs rhs mask = Shuffle (lhs, rhs, mask)

let undef ty = Undef ty

let insert_element vec elt idx = InsertElement (vec, elt, idx)

let broadcast base_num scalar size =
  let vec_ty = Types.vec base_num size in
  let* un = undef vec_ty in
  let* lhs = insert_element un scalar I32.zero in
  shuffle lhs un (mask size (Array.make (Size.to_int size) 0))

let rec block cmds =
  match cmds with
  | [] -> unit
  | [cmd] -> cmd
  | cmd :: cmds ->
      let* _ = cmd in
      block cmds

module Stack = Syntax.Stack

let local var f = Local (var, f)

let ( let*:: ) = local

let end_frame k = End_frame k

let fundecl name typ body = Syntax.Fundecl (Syntax.fundecl name typ body)

let intrinsic name typ = { intrinsic_name = name; intrinsic_sg = typ }

module Run = struct
  open Run

  module type BA = Run.BA

  module I64_ba = Run.I64_ba
  module I32_ba = Run.I32_ba
  module I16_ba = Run.I16_ba
  module I8_ba = Run.I8_ba
  module F64_ba = Run.F64_ba
  module F32_ba = Run.F32_ba

  type ('suplex, 'ocaml) rel = ('suplex, 'ocaml) Run.rel

  type ('suplex, 'ocaml) rel_vec = ('suplex, 'ocaml) Run.rel_vec

  type ('suplex, 'ocaml) fn_rel = ('suplex, 'ocaml) Run.fn_rel

  type 'a opaque = 'a Run.opaque

  type 'a module_ = 'a Run.module_

  let empty = Vec []

  let ( |+ ) (Vec v) (Rel r) = Vec (r :: v)

  let unit = Rel Full_rel_unit

  let bool = Rel Full_rel_bool

  let base_num r = Rel (Full_rel_num (Full_rel_base r))

  let i64 = base_num Full_rel_int64

  let i32 = base_num Full_rel_int32

  let i16 = base_num Full_rel_int16

  let i8 = base_num Full_rel_int8

  let i1 = base_num Full_rel_int1

  let f64 = base_num Full_rel_float64

  let f32 = base_num Full_rel_float32

  let bigarray_i64 = Rel Full_rel_ba_i64

  let bigarray_i32 = Rel Full_rel_ba_i32

  let bigarray_i16 = Rel Full_rel_ba_i16

  let bigarray_i8 = Rel Full_rel_ba_i8

  let bigarray_f64 = Rel Full_rel_ba_f64

  let bigarray_f32 = Rel Full_rel_ba_f32

  let string = Rel Full_rel_string

  let array_raw (Rel r) = Rel (Full_rel_arr_unk r)

  let array len (Rel r) = Rel (Full_rel_arr_cst (len, r))

  let mallocd_strct strct (Vec spec) =
    Rel (Full_rel_mallocd_struct (strct, spec))

  let opaque_mallocd_strct strct = Rel (Full_rel_opaque_mallocd_struct strct)

  let strct strct (Vec spec) = Rel (Full_rel_struct (strct, spec))

  let returning (Rel r) = Fn (Fn_returning r)

  let ( @-> ) dom (Fn range) =
    let (Rel dom) = subst dom in
    Fn (Fn_arrow (dom, range))

  let add_intrinsic = Run.add_intrinsic

  let add_fundecl = Run.add_fundecl

  let main = Run.main

  let jit_module = Run.jit_module

  let jit_program = Run.jit_program

  let jit = Run.jit
end

module Externals = struct
  external fail : unit -> unit = "failwith" [@@ocaml.warning "-32"]
end
