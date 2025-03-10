(** {2 Types} *)

(** The type tag of 64-bits signed integers. *)
type i64 = I64

(** The type tag of 32-bits signed integers. *)
type i32 = I32

(** The type tag of 16-bits signed integers. *)
type i16 = I16

(** The type tag of 8-bits signed integers. *)
type i8 = I8

(** The type tag of single-precision floating-point numbers. *)
type f32 = F32

(** The type tag of double-precision floating-point numbers. *)
type f64 = F64

(** The type tag of pointers. *)
type 'a ptr = Ptr

(** [('a, 'c) arr] is the abstract type of arrays. The first type parameter is
    the type of elements while the second type parameter encodes whether the
    array has a statically known size or not. *)
type (!'a, 'c) arr = Arr

type ('a, 'sz) vec = Vector

type 'a record = Record

(** ['a base_numerical] is the type of all base {b suplex} numerical types. *)
type 'a base_numerical =
  | I64_num : i64 base_numerical
  | I32_num : i32 base_numerical
  | I16_num : i16 base_numerical
  | I8_num : i8 base_numerical
  | F64_num : f64 base_numerical
  | F32_num : f32 base_numerical

type 'a numerical =
  | Base_num : 'a base_numerical -> 'a numerical
  | Vec_num :
      { base : 'a base_numerical; numel : 'sz Size.t }
      -> ('a, 'sz) vec numerical

(** {2 Relationship between numerical types and OCaml values *)

type (_, _) num_rel =
  | I64_rel : (i64, int64) num_rel
  | I32_rel : (i32, int32) num_rel
  | I16_rel : (i16, int) num_rel
  | I8_rel : (i8, int) num_rel
  | F64_rel : (f64, float) num_rel
  | F32_rel : (f32, float) num_rel

(** Classifies base numerical types into floating-point kind [`fp] or integer
    kind [`int]. *)
let base_numerical_kind : type a. a base_numerical -> [ `fp | `int ] =
 fun n ->
  match n with
  | I64_num -> `int
  | I32_num -> `int
  | I16_num -> `int
  | I8_num -> `int
  | F32_num -> `fp
  | F64_num -> `fp

(** Classifies base numerical types into floating-point kind [`fp] or integer
    kind [`int]. *)
let numerical_kind : type a. a numerical -> [ `fp | `int ] =
 fun n ->
  match n with
  | Base_num n -> base_numerical_kind n
  | Vec_num { base; _ } -> base_numerical_kind base

(** ['a typ] is the type of {b suplex} types. *)
type 'a typ =
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

(** [('elim, 't_acc, 't, 'u) record] is the type of record descriptors. The type
    parameters are as follows:
    - ['elim] is the type of eliminators for this record type
    - ['t_acc] is a type-level accumulator used internally, you can ignore it
    - ['t] is the type of elements held in the record
    - ['u] is the final type of the record, which will be visible after {!seal}
      is called. *)
and ('elim, 't_acc, 't, 'u) record_desc =
  | Record_empty : (unit, unit Vec.t, 't Vec.t, 'u record) record_desc
  | Record_field :
      { field : ('a, 'u record) field;
        index : int;
        rest : ('e, 't_acc Vec.t, 't Vec.t, 'u record) record_desc
      }
      -> ( 'e * ('a, 'u record) field,
           ('a * 't_acc) Vec.t,
           't Vec.t,
           'u record )
         record_desc
  | Record_fix :
      int * ('u record typ -> ('b, 'd Vec.t, 'd Vec.t, 'u record) record_desc)
      -> ('b, 'd Vec.t, 'd Vec.t, 'u record) record_desc

(** The type of record fields. *)
and ('a, 't) field = Field : { name : string; ty : 'a typ } -> ('a, 't) field

(** For some values, we can take their addresses. *)
and 'a addressable =
  (* | Addressable_record : 'a record typ -> 'a record addressable *)
  | Addressable_array : ('a, [ `cst ]) arr addressable
  | Addressable_record : 'a record addressable

(** {2 Expressions} *)

and _ expr =
  | Var : 'a typed_llvm Hmap.key -> 'a expr
  | Let : 'a expr * ('a expr -> 'b expr) -> 'b expr
  | Unit : unit expr
  | True : bool expr
  | False : bool expr
  | String : { strz : bool; str : string } -> i8 ptr expr
  | And : bool expr * bool expr -> bool expr
  | Or : bool expr * bool expr -> bool expr
  | I64 : int64 -> i64 expr
  | I32 : int32 -> i32 expr
  | I16 : int -> i16 expr
  | I8 : int -> i8 expr
  | F64 : float -> f64 expr
  | F32 : float -> f32 expr
  | Add : 'a numerical * 'a expr * 'a expr -> 'a expr
  | Sub : 'a numerical * 'a expr * 'a expr -> 'a expr
  | Mul : 'a numerical * 'a expr * 'a expr -> 'a expr
  | Div : 'a numerical * 'a expr * 'a expr -> 'a expr
  | Neg : 'a numerical * 'a expr -> 'a expr
  | Lt : 'a numerical * 'a expr * 'a expr -> bool expr
  | Le : 'a numerical * 'a expr * 'a expr -> bool expr
  | Eq : 'a numerical * 'a expr * 'a expr -> bool expr
  | PtrEq : 'a ptr expr * 'a ptr expr -> bool expr
  | Store : 'a ptr expr * 'a expr -> unit expr
  | Load : 'a ptr expr -> 'a expr
  | NullPtr : 'a typ -> 'a ptr expr
  | IsNull : 'a ptr expr -> bool expr
  | AddrOf : 'a addressable * 'a expr -> 'a ptr expr
  | Get : ('a, 'c) arr expr * i64 expr -> 'a expr
  | GetAddr : ('a, 'c) arr expr * i64 expr -> 'a ptr expr
  | Set : ('a, 'c) arr expr * i64 expr * 'a expr -> unit expr
  | GetField : ('a, 'u record) field * 'u record expr -> 'a expr
  | GetFieldAddr : ('a, 'u record) field * 'u record expr -> 'a ptr expr
  | SetField : ('a, 'u record) field * 'u record expr * 'a expr -> unit expr
  | Cond : bool expr * 'a expr * 'a expr -> 'a expr
  | For :
      { init : i64 expr;
        pred : i64 expr -> bool expr;
        step : i64 expr -> i64 expr;
        body : i64 expr -> unit expr
      }
      -> unit expr
  | Foldi :
      { init : i64 expr;
        acc : 'acc expr;
        pred : i64 expr -> 'acc expr -> bool expr;
        step : i64 expr -> i64 expr;
        body : i64 expr -> 'acc expr -> 'acc expr
      }
      -> 'acc expr
  | While : bool expr * unit expr -> unit expr
  | Switch :
      { e : i64 expr; cases : (int64 * 'a expr) list; default : 'a expr }
      -> 'a expr
  | Fundecl : 'a fundecl -> 'a fn expr
  | Call : 'a fn expr * ('a, 'b expr) args -> 'b expr
  | Fail : string -> 'a expr
  | Malloc : 'a typ -> 'a ptr expr
  | Malloc_array : 'a typ * i64 expr -> ('a, [ `unk ]) arr expr
  | Free : 'a ptr expr -> unit expr
  | Free_array : ('a, [ `unk ]) arr expr -> unit expr
  | Const_array : ('s, 'o) num_rel * 'o array -> ('s, [ `cst ]) arr expr
  | Trunc : 'a numerical * 'b numerical * 'a expr -> 'b expr
  | SExt : 'a numerical * 'b numerical * 'a expr -> 'b expr
  | ZExt : 'a numerical * 'b numerical * 'a expr -> 'b expr
  | ToF32 : 'a base_numerical * 'a expr -> f32 expr
  | ToF64 : 'a base_numerical * 'a expr -> f64 expr
  | OfF32 : 'a base_numerical * f32 expr -> 'a expr
  | OfF64 : 'a base_numerical * f64 expr -> 'a expr
  | VecToF32 : 'a base_numerical * ('a, 'sz) vec expr -> (f32, 'sz) vec expr
  | VecToF64 : 'a base_numerical * ('a, 'sz) vec expr -> (f64, 'sz) vec expr
  | VecOfF32 : 'a base_numerical * (f32, 'sz) vec expr -> ('a, 'sz) vec expr
  | VecOfF64 : 'a base_numerical * (f64, 'sz) vec expr -> ('a, 'sz) vec expr

and 'a typed_llvm = { value : Llvm.llvalue; ty : 'a typ }

(** {2 Function signature specification} *)

and _ fn =
  | Returning : 'a typ -> 'a expr fn
  | Arrow : 'a typ * 'b fn -> ('a expr -> 'b) fn

and ('a, 'b) args =
  | Args_nil : ('a expr, 'a expr) args
  | Args_cons : 'a expr * ('b, 'ret) args -> ('a expr -> 'b, 'ret) args

and 'a fundecl =
  { name : string;
    sg : 'a fn;  (** Signature *)
    body : ('a fn expr -> 'a) stack  (** Stack specification *)
  }

(** {2 Descriptors for stack variables} *)

and 'a stack_var =
  | SV_unit : unit ptr stack_var
  | SV_bool : bool ptr stack_var
  | SV_num : 'a numerical -> 'a ptr stack_var
  | SV_ptr : 'a typ -> 'a ptr ptr stack_var
  | SV_arr : 'a typ * i64 expr -> ('a, [ `unk ]) arr stack_var
  | SV_arr_cst : 'a typ * int64 -> ('a, [ `cst ]) arr stack_var
  | SV_strct :
      (_, 'd Vec.t, 'd Vec.t, 't record) record_desc
      -> 't record stack_var

(** {2 Descriptors for stacks} *)

and _ stack =
  | Local : 'a stack_var * ('a expr -> 'b stack) -> 'b stack
  | End_frame : 's -> 's stack

(** {2 Mapping variables to compiled expressions} *)
type environment = Hmap.t

(** {2 Description of function stacks *)

module Stack = struct
  let end_frame f = End_frame f

  let ( @+ ) v f = Local (v, f)

  let unit = SV_unit

  let bool = SV_bool

  let num n = SV_num n

  let base_num n = SV_num (Base_num n)

  let i64 = base_num I64_num

  let i32 = base_num I32_num

  let i16 = base_num I16_num

  let i8 = base_num I8_num

  let f64 = base_num F64_num

  let f32 = base_num F32_num

  let ptr ty = SV_ptr ty

  let arr ty len = SV_arr (ty, len)

  let arr_cst ty len =
    if len < 0L then invalid_arg "arr_cst: negative size" ;
    SV_arr_cst (ty, len)

  let strct r = SV_strct r
end

let fundecl name sg body = { name; sg; body }
