module Vec = Vec
module Types = Types
module Syntax = Syntax
module Compile = Compile
module Intf = Intf
module Type_system = Type_system

module Llvm_impl : sig
  include Intf.S with type Exec.cfg = Llvm_executionengine.llcompileroptions

  val set_debug_metadata : string -> unit
end =
  Llvm_impl

type i64 = Type_system.i64 = I64

type i32 = Type_system.i32 = I32

type i16 = Type_system.i16 = I16

type i8 = Type_system.i8 = I8

type f32 = Type_system.f32 = F32

type f64 = Type_system.f64 = F64

type 'a numerical = 'a Type_system.numerical =
  | I64_num : Type_system.i64 numerical
  | I32_num : Type_system.i32 numerical
  | I16_num : Type_system.i16 numerical
  | I8_num : Type_system.i8 numerical
  | F32_num : Type_system.f32 numerical
  | F64_num : Type_system.f64 numerical

type 'a ptr = 'a Syntax.ptr = Ptr

type 'a record = 'a Syntax.record = Record

open Syntax

type 'a expr = 'a Syntax.expr

module type Numerical = sig
  (** [t] is the numerical type. *)
  type t

  (** [v] is the type of constant values. *)
  type v

  (** Witness that [t] is a numerical type. *)
  val n : t Syntax.numerical

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

  val v : v -> t expr

  val n : t numerical

  val zero : t expr

  val one : t expr
end) : Numerical with type t = Num.t and type v = Num.v = struct
  type t = Num.t

  type v = Num.v

  let n = Num.n

  let v = Num.v

  let zero = Num.zero

  let one = Num.one

  let add a b = Add (Num.n, a, b)

  let sub a b = Sub (Num.n, a, b)

  let mul a b = Mul (Num.n, a, b)

  let div a b = Div (Num.n, a, b)

  let neg a = Neg (Num.n, a)

  let lt a b = Lt (Num.n, a, b)

  let le a b = Le (Num.n, a, b)

  let eq a b = Eq (Num.n, a, b)
end

module I64 = Make_numerical (struct
  type t = i64

  type v = int64

  let v x = I64 x

  let n = I64_num

  let zero = v 0L

  let one = v 1L
end)

module I32 = Make_numerical (struct
  type t = i32

  type v = int32

  let v x = I32 x

  let n = I32_num

  let zero = v 0l

  let one = v 1l
end)

module I16 = Make_numerical (struct
  type t = i16

  type v = int

  let v x = I16 x

  let n = I16_num

  let zero = v 0

  let one = v 1
end)

module I8 = Make_numerical (struct
  type t = i8

  type v = int

  let v x = I8 x

  let n = I8_num

  let zero = v 0

  let one = v 1
end)

module F64 = Make_numerical (struct
  type t = f64

  type v = float

  let v x = F64 x

  let n = F64_num

  let zero = v 0.0

  let one = v 1.0
end)

module F32 = Make_numerical (struct
  type t = f32

  type v = float

  let v x = F32 x

  let n = F32_num

  let zero = v 0.0

  let one = v 1.0
end)

let ( let* ) m f = Let (m, f)

let unit = Unit

let tt = True

let ff = False

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

let arg a rest = Args_cons (a, rest)

let addr_of arr = AddrOf (Addressable_array, arr)

let ptr_eq a b = PtrEq (a, b)

let fail msg = Fail msg

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
    Rel (Full_rel_malloced_struct (strct, spec))

  let opaque_mallocd_strct strct = Rel (Full_rel_opaque_malloced_struct strct)

  let strct strct (Vec spec) = Rel (Full_rel_struct (strct, spec))

  let returning (Rel r) = Fn (Fn_returning r)

  let ( @-> ) dom (Fn range) =
    let (Rel dom) = subst dom in
    Fn (Fn_arrow (dom, range))

  let run_module = Run.run_module

  let run_program = Run.run_program

  let run = Run.run
end
