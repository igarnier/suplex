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

type 'a record = Record

(** ['a numerical] is the type of all {b suplex} numerical types. *)
type 'a numerical =
  | I64_num : i64 numerical
  | I32_num : i32 numerical
  | I16_num : i16 numerical
  | I8_num : i8 numerical
  | F64_num : f64 numerical
  | F32_num : f32 numerical

(** Pretty-printing numerical types. *)
let pp_numerical : type a. Format.formatter -> a numerical -> unit =
 fun (type a) fmtr (n : a numerical) ->
  let open Format in
  match n with
  | I64_num -> fprintf fmtr "i64"
  | I32_num -> fprintf fmtr "i32"
  | I16_num -> fprintf fmtr "i16"
  | I8_num -> fprintf fmtr "i8"
  | F32_num -> fprintf fmtr "f32"
  | F64_num -> fprintf fmtr "f64"

(** Classifies numerical types into floating-point kind [`fp] or integer kind
    [`int]. *)
let numerical_kind : type a. a numerical -> [ `fp | `int ] =
 fun n ->
  match n with
  | I64_num -> `int
  | I32_num -> `int
  | I16_num -> `int
  | I8_num -> `int
  | F32_num -> `fp
  | F64_num -> `fp

let numerical_eq : type a b. a numerical -> b numerical -> (a, b) Type.eq option
    =
 fun n1 n2 ->
  match (n1, n2) with
  | (I64_num, I64_num) -> Some Equal
  | (I32_num, I32_num) -> Some Equal
  | (I16_num, I16_num) -> Some Equal
  | (I8_num, I8_num) -> Some Equal
  | (F64_num, F64_num) -> Some Equal
  | (F32_num, F32_num) -> Some Equal
  | _ -> None

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

  let ptr ty = SV_ptr ty

  let arr ty len = SV_arr (ty, len)

  let arr_cst ty len =
    if len < 0L then invalid_arg "arr_cst: negative size" ;
    SV_arr_cst (ty, len)

  let strct r = SV_strct r
end

let fundecl name sg body = { name; sg; body }
