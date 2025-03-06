module Vec = Vec
module Types = Types
module Syntax = Syntax
module Compile = Compile
open Syntax

type i64 = Syntax.i64 = I64

type i32 = Syntax.i32 = I32

type i16 = Syntax.i16 = I16

type i8 = Syntax.i8 = I8

type f32 = Syntax.f32 = F32

type f64 = Syntax.f64 = F64

type 'a numerical = 'a Syntax.numerical =
  | I64_num : i64 numerical
  | I32_num : i32 numerical
  | I16_num : i16 numerical
  | I8_num : i8 numerical
  | F64_num : f64 numerical
  | F32_num : f32 numerical

type 'a typ = 'a Syntax.typ

type 'a ptr = 'a Syntax.ptr = Ptr

type 'a record = 'a Syntax.record = Record

type ('a, 'b, 'c, 'd) record_desc = ('a, 'b, 'c, 'd) Syntax.record_desc

type ('f, 'r) field = ('f, 'r) Syntax.field =
  | Field : { name : string; ty : 'a typ } -> ('a, 't) field

type ('a, 'k) arr = ('a, 'k) Syntax.arr

type 'a fn = 'a Syntax.fn

type 'a stack = 'a Syntax.stack

type 'a stack_var = 'a Syntax.stack_var

type 'a expr = 'a Syntax.expr

type 'a fundecl = 'a Syntax.fundecl

type ('a, 'b) args = ('a, 'b) Syntax.args

type ('s, 'o) num_rel = ('s, 'o) Syntax.num_rel

module type Numerical = sig
  (** [t] is the numerical type. *)
  type t

  (** [v] is the type of constant values. *)
  type v

  (** Witness that [t] is a numerical type. *)
  val rel : (t, v) Syntax.num_rel

  (** [v c] is a constant equal to [c]. *)
  val v : v -> t expr

  val add : t expr -> t expr -> t expr

  val sub : t expr -> t expr -> t expr

  val mul : t expr -> t expr -> t expr

  val div : t expr -> t expr -> t expr

  val neg : t expr -> t expr

  val lt : t expr -> t expr -> bool expr

  val le : t expr -> t expr -> bool expr

  val eq : t expr -> t expr -> bool expr

  val zero : t expr

  val one : t expr
end

module Make_numerical (Num : sig
  type t

  type v

  val rel : (t, v) num_rel

  val zero : t expr

  val one : t expr
end) : Numerical with type t = Num.t and type v = Num.v = struct
  type t = Num.t

  type v = Num.v

  let rel = Num.rel

  let n = Compile.numerical_of_num_rel rel

  let v x = Compile.const rel x

  let zero = Num.zero

  let one = Num.one

  let add a b = Add (n, a, b)

  let sub a b = Sub (n, a, b)

  let mul a b = Mul (n, a, b)

  let div a b = Div (n, a, b)

  let neg a = Neg (n, a)

  let lt a b = Lt (n, a, b)

  let le a b = Le (n, a, b)

  let eq a b = Eq (n, a, b)
end

module I64 = Make_numerical (struct
  type t = i64

  type v = int64

  let v x : t expr = I64 x

  let rel = I64_rel

  let zero = v 0L

  let one = v 1L
end)

module I32 = Make_numerical (struct
  type t = i32

  type v = int32

  let v x : t expr = I32 x

  let rel = I32_rel

  let zero = v 0l

  let one = v 1l
end)

module I16 = Make_numerical (struct
  type t = i16

  type v = int

  let v x : t expr = I16 x

  let rel = I16_rel

  let zero = v 0

  let one = v 1
end)

module I8 = Make_numerical (struct
  type t = i8

  type v = int

  let v x : t expr = I8 x

  let rel = I8_rel

  let zero = v 0

  let one = v 1
end)

module F64 = Make_numerical (struct
  type t = f64

  type v = float

  let v x : t expr = F64 x

  let rel = F64_rel

  let zero = v 0.0

  let one = v 1.0
end)

module F32 = Make_numerical (struct
  type t = f32

  type v = float

  let v x : t expr = F32 x

  let rel = F32_rel

  let zero = v 0.0

  let one = v 1.0
end)

module type BA = Run.BA

module I64_ba = Run.I64_ba
module I32_ba = Run.I32_ba
module I16_ba = Run.I16_ba
module I8_ba = Run.I8_ba
module F64_ba = Run.F64_ba
module F32_ba = Run.F32_ba

let ( let* ) m f = Let (m, f)

let unit = Unit

let tt = True

let ff = False

let const_array (type t v) (module N : Numerical with type t = t and type v = v)
    len =
  Const_array (N.rel, len)

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

let can_be_truncated : type a b.
    a numerical -> b numerical -> (unit, string) Result.t =
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
        Types.pp_numerical
        n1
        Types.pp_numerical
        n2

let trunc src dst v =
  match can_be_truncated src dst with
  | Error e -> failwith e
  | Ok () -> Trunc (src, dst, v)

let can_be_extended : type a b.
    a numerical -> b numerical -> (unit, string) Result.t =
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

  let i64 = Rel Full_rel_int64

  let i32 = Rel Full_rel_int32

  let i16 = Rel Full_rel_int16

  let i8 = Rel Full_rel_int8

  let f64 = Rel Full_rel_float64

  let f32 = Rel Full_rel_float32

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

  let add_fundecl = Run.add_fundecl

  let main = Run.main

  let run_module = Run.run_module

  let run_program = Run.run_program

  let run = Run.run
end

module Externals = struct
  external fail : unit -> unit = "failwith" [@@ocaml.warning "-32"]
end
