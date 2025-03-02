type i64 = I64

type i32 = I32

type i16 = I16

type i8 = I8

type f32 = F32

type f64 = F64

type 'a numerical =
  | I64_num : i64 numerical
  | I32_num : i32 numerical
  | I16_num : i16 numerical
  | I8_num : i8 numerical
  | F64_num : f64 numerical
  | F32_num : f32 numerical

type 'a typ

type 'a ptr

type 'a record

type ('a, 'b, 'c, 'd) record_desc

type ('a, 't) field = private
  | Field : { name : string; ty : 'a typ } -> ('a, 't) field

type ('a, 'k) arr

type 'a fn

type 'a stack

type 'a stack_var

type 'a expr

type 'a fundecl

type ('a, 'b) args

type ('s, 'o) num_rel

module Types : sig
  type nonrec 'a typ = 'a typ

  val field_name : ('a, 'b) field -> string

  val field_type : ('a, 'b) field -> 'a typ

  val pp_fn : int list -> Format.formatter -> 'a fn -> unit

  val pp_typ : Format.formatter -> 'a typ -> unit

  val pp_field : Format.formatter -> ('a, 'b) field -> unit

  val type_eq : 'a typ -> 'b typ -> bool

  val record_eq :
    ('a, 'b, 'c, 'u) record_desc -> ('d, 'e, 'f, 'v) record_desc -> bool

  val field_eq : ('a, 'b) field -> ('c, 'd) field -> bool

  val field_index : ('a, 'u) field -> 'u typ -> (int option, string) Result.t

  val unit : unit typ

  val bool : bool typ

  val i64 : i64 typ

  val i32 : i32 typ

  val i16 : i16 typ

  val i8 : i8 typ

  val f32 : f32 typ

  val f64 : f64 typ

  val ptr : 'a typ -> 'a ptr typ

  val arr : 'a typ -> ('a, [ `unk ]) arr typ

  val arr_cst : 'a typ -> int64 -> ('a, [ `cst ]) arr typ

  val empty_rec : (unit, unit Vec.t, 'a Vec.t, 'b record) record_desc

  val field : string -> 'a typ -> ('a, 'b) field

  val next_index : ('x, 'y, 'z, 'u) record_desc -> int

  val ( |+ ) :
    ('a, 'b Vec.t, 'c Vec.t, 'd record) record_desc ->
    ('e, 'd record) field ->
    ( 'a * ('e, 'd record) field,
      ('e * 'b) Vec.t,
      'c Vec.t,
      'd record )
    record_desc

  val gensym : unit -> int

  val fix :
    ('a record typ -> ('b, 'c Vec.t, 'c Vec.t, 'a record) record_desc) ->
    ('b, 'c Vec.t, 'c Vec.t, 'a record) record_desc

  val seal : ('a, 'b Vec.t, 'b Vec.t, 'c record) record_desc -> 'c record typ

  val projs : ('elim, 'x Vec.t, 'x Vec.t, 'u) record_desc -> 'elim

  val returning : 'a typ -> 'a expr fn

  val ( @-> ) : 'a typ -> 'b fn -> ('a expr -> 'b) fn

  val fn_eq : 'a fn -> 'b fn -> bool

  val assert_const_array :
    ('a, [ `cst ]) arr typ -> expected_size:Int64.t -> unit
end

(** The module type of numerical types and their operations. *)
module type Numerical = sig
  (** [t] is the numerical type. *)
  type t

  (** [v] is the OCaml representation of constant values. *)
  type v

  (** Witness that [t] is a numerical type represented by [v]. *)
  val rel : (t, v) num_rel

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

module type BA = sig
  type elt

  type s

  val r :
    ( (unit * (i64, s record) field) * ((elt, [ `unk ]) arr, s record) field,
      ((elt, [ `unk ]) arr * (i64 * unit)) Vec.t,
      ((elt, [ `unk ]) arr * (i64 * unit)) Vec.t,
      s record )
    record_desc

  val s : s record typ

  val data : ((elt, [ `unk ]) arr, s record) field

  val dim : (i64, s record) field
end

module I64 : Numerical with type t = i64 and type v = int64

module I32 : Numerical with type t = i32 and type v = int32

module I16 : Numerical with type t = i16 and type v = int

module I8 : Numerical with type t = i8 and type v = int

module F64 : Numerical with type t = f64 and type v = float

module F32 : Numerical with type t = f32 and type v = float

module I64_ba : BA with type elt = i64

module I32_ba : BA with type elt = i32

module I16_ba : BA with type elt = i16

module I8_ba : BA with type elt = i8

module F64_ba : BA with type elt = f64

module F32_ba : BA with type elt = f32

(** Sequencing operator. *)
val ( let* ) : 'a expr -> ('a expr -> 'b expr) -> 'b expr

(** [unit] is the unit value. *)
val unit : unit expr

(** [tt] is the [true] boolean constant. *)
val tt : bool expr

(** [ff] is the [false] boolean constant. *)
val ff : bool expr

(** [const_array numty arr] is a global constant array containing values
    specified by [arr] *)
val const_array :
  (module Numerical with type t = 't and type v = 'v) ->
  'v array ->
  ('t, [ `cst ]) arr expr

(** [string ?strz str] constructs a constant string with contents [str]. If [z]
    is set to false, the resulting string is not zero-terminated. *)
val string : ?strz:bool -> string -> i8 ptr expr

(** Boolean conjunction. *)
val ( &&& ) : bool expr -> bool expr -> bool expr

(** Boolean disjunction. *)
val ( ||| ) : bool expr -> bool expr -> bool expr

(** [p <-- v] stores the result of the evaluation of [v] at the address computed
    by [p] *)
val ( <-- ) : 'a ptr expr -> 'a expr -> unit expr

(** [~*p] loads the value stored at the address computed by [p] *)
val ( ~* ) : 'a ptr expr -> 'a expr

(** [is_null p] is true if [p] is the null pointer *)
val is_null : 'a ptr expr -> bool expr

(** [null_ptr ty] is the null pointer of type [ty] *)
val null_ptr : 'a typ -> 'a ptr expr

(** [arr.%[i]] is the [i]-th element of the array [arr] *)
val ( .%[] ) : ('a, 'b) arr expr -> i64 expr -> 'a expr

(** [arr.%[i]] is the address of the [i]-th element of the array [arr] *)
val ( .%&[] ) : ('a, 'b) arr expr -> i64 expr -> 'a ptr expr

(** [arr.%[i] <- v] stores [v] at the [i]-th element of the array [arr] *)
val ( .%[]<- ) : ('a, 'b) arr expr -> i64 expr -> 'a expr -> unit expr

(** [record.%{f}] is the value of the field [f] in the record [record] *)
val ( .%{} ) : 'a record expr -> ('b, 'a record) field -> 'b expr

(** [record.&%{f}] is the address of the field [f] in the record [record] *)
val ( .%&{} ) : 'a record expr -> ('b, 'a record) field -> 'b ptr expr

(** [record.%{f} <- v] stores [v] at the field [f] in the record [record] *)
val ( .%{}<- ) : 'a record expr -> ('b, 'a record) field -> 'b expr -> unit expr

(** [if_ c iftrue iffalse] constructs a conditional expression. *)
val if_ : bool expr -> 'a expr -> 'a expr -> 'a expr

(** [for_ ~init ~pred ~step body] is a for loop with initial value [init],
    predicate [pred], step function [step], and body [body]. *)
val for_ :
  init:i64 expr ->
  pred:(i64 expr -> bool expr) ->
  step:(i64 expr -> i64 expr) ->
  (i64 expr -> unit expr) ->
  unit expr

(** [foldi ~init ~acc ~pred ~step ~f] is a fold over the integers starting at
    [init], with initial accumulator [acc], stopping predicate [pred], step
    function [step], and folding function [f]. *)
val foldi :
  init:i64 expr ->
  acc:'a expr ->
  pred:(i64 expr -> 'a expr -> bool expr) ->
  step:(i64 expr -> i64 expr) ->
  (i64 expr -> 'a expr -> 'a expr) ->
  'a expr

(** [while_ c body] is a while loop with predicate [c] and body [body]. *)
val while_ : bool expr -> unit expr -> unit expr

(** [switch v ~default cases] is a switch statement on value [v] with default
    case [default] and cases [cases]. A case is a pair of a constant integer tag
    and the associated expression. *)
val switch : i64 expr -> default:'a expr -> (int64 * 'a expr) list -> 'a expr

(** [call f args] calls the function [f] with the vector of arguments [args]. *)
val call : 'a fn expr -> ('a, 'b expr) args -> 'b expr

(** [call1 f a] is a shorthand for [call f (arg a empty_args)] *)
val call1 : ('a expr -> 'b expr) fn expr -> 'a expr -> 'b expr

(** [call2 f a b] is a shorthand for [call f (arg a (arg b empty_args))] *)
val call2 :
  ('a expr -> 'b expr -> 'c expr) fn expr -> 'a expr -> 'b expr -> 'c expr

(** [call3 f a b c] is a shorthand for
    [call f (arg a (arg b (arg c empty_args)))] *)
val call3 :
  ('a expr -> 'b expr -> 'c expr -> 'd expr) fn expr ->
  'a expr ->
  'b expr ->
  'c expr ->
  'd expr

val arg : 'a expr -> ('b, 'c) args -> ('a expr -> 'b, 'c) args

val empty_args : ('a expr, 'a expr) args

val addr_of_arr : ('a, [ `cst ]) arr expr -> ('a, [ `cst ]) arr ptr expr

val addr_of_rec : 'a record expr -> 'a record ptr expr

val ptr_eq : 'a ptr expr -> 'a ptr expr -> bool expr

val fail : string -> 'a expr

val malloc : 'a typ -> 'a ptr expr

val malloc_array : 'a typ -> i64 expr -> ('a, [ `unk ]) arr expr

val free : 'a ptr expr -> unit expr

val free_array : ('a, [ `unk ]) arr expr -> unit expr

val block : unit expr list -> unit expr

module Stack : sig
  val end_frame : 'a -> 'a stack

  val ( @+ ) : 'a stack_var -> ('a expr -> 'b stack) -> 'b stack

  val unit : unit ptr stack_var

  val bool : bool ptr stack_var

  val num : 'a numerical -> 'a ptr stack_var

  val i64 : i64 ptr stack_var

  val i32 : i32 ptr stack_var

  val i16 : i16 ptr stack_var

  val i8 : i8 ptr stack_var

  val f64 : f64 ptr stack_var

  val f32 : f32 ptr stack_var

  val ptr : 'a typ -> 'a ptr ptr stack_var

  val arr : 'a typ -> i64 expr -> ('a, [ `unk ]) arr stack_var

  val arr_cst : 'a typ -> int64 -> ('a, [ `cst ]) arr stack_var

  val strct :
    ('a, 'b Vec.t, 'b Vec.t, 'c record) record_desc -> 'c record stack_var
end

val local : 'a stack_var -> ('a expr -> 'b stack) -> 'b stack

val ( let*:: ) : 'a stack_var -> ('a expr -> 'b stack) -> 'b stack

val end_frame : 'a -> 'a stack

val fundecl : string -> 'a fn -> ('a fn expr -> 'a) stack -> 'a fn expr

module Run : sig
  type ('suplex, 'ocaml) rel

  type ('suplex, 'ocaml) rel_vec

  type ('suplex, 'ocaml) fn_rel

  type 'a opaque

  type 'a module_

  val empty : (unit Vec.t, unit Vec.t) rel_vec

  val ( |+ ) :
    ('a Vec.t, 'b Vec.t) rel_vec ->
    ('c expr, 'd) rel ->
    (('c * 'a) Vec.t, ('d * 'b) Vec.t) rel_vec

  val unit : (unit expr, unit) rel

  val bool : (bool expr, bool) rel

  val i64 : (i64 expr, int64) rel

  val i32 : (i32 expr, int32) rel

  val i16 : (i16 expr, int) rel

  val i8 : (i8 expr, int) rel

  val f64 : (f64 expr, float) rel

  val f32 : (f32 expr, float) rel

  val bigarray_i64 :
    ( I64_ba.s record expr,
      (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t )
    rel

  val bigarray_i32 :
    ( I32_ba.s record expr,
      (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t )
    rel

  val bigarray_i16 :
    ( I16_ba.s record expr,
      (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t )
    rel

  val bigarray_i8 :
    ( I8_ba.s record expr,
      (int, Bigarray.int8_signed_elt, Bigarray.c_layout) Bigarray.Array1.t )
    rel

  val bigarray_f64 :
    ( F64_ba.s record expr,
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t )
    rel

  val bigarray_f32 :
    ( F32_ba.s record expr,
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t )
    rel

  val string : (i8 ptr expr, string) rel

  val array_raw : ('a expr, 'b) rel -> (('a, [ `unk ]) arr expr, 'b Seq.t) rel

  val array :
    int -> ('a expr, 'b) rel -> (('a, [ `cst ]) arr expr, 'b Seq.t) rel

  val mallocd_strct :
    ('a, 'b Vec.t, 'b Vec.t, 'c record) record_desc ->
    ('b Vec.t, 'd Vec.t) rel_vec ->
    ('c record expr, 'd Vec.t) rel

  val opaque_mallocd_strct :
    ('a, 'b Vec.t, 'b Vec.t, 'c record) record_desc ->
    ('c record expr, 'c opaque) rel

  val strct :
    ('a, 'b Vec.t, 'b Vec.t, 'c record) record_desc ->
    ('b Vec.t, 'd Vec.t) rel_vec ->
    ('c record expr, 'd Vec.t) rel

  val returning : ('a expr, 'b) rel -> ('a expr, 'b) fn_rel

  val ( @-> ) :
    ('a expr, 'b) rel -> ('c, 'd) fn_rel -> ('a expr -> 'c, 'b -> 'd) fn_rel

  val add_fundecl :
    string ->
    ('a, 'b -> 'c) fn_rel ->
    ('a fn expr -> 'a) stack ->
    ('a fn expr -> 'd module_) ->
    ('d * ('b -> 'c)) module_

  val main :
    string ->
    ('a, 'b -> 'c) fn_rel ->
    ('a fn expr -> 'a) stack ->
    ('b -> 'c) module_

  val run_module :
    ?debug:bool ->
    ?cfg:Llvm_executionengine.llcompileroptions ->
    ?state:Compile.llvm_state ->
    'a module_ ->
    'a

  val run_program :
    ?cfg:Llvm_executionengine.llcompileroptions ->
    ?state:Compile.llvm_state ->
    'a fundecl ->
    ('a, 'b -> 'c) fn_rel ->
    'b ->
    'c

  val run :
    ?cfg:Llvm_executionengine.llcompileroptions ->
    ?fname:string ->
    ('a, 'b -> 'c) fn_rel ->
    ('a fn expr -> 'a) stack ->
    'b ->
    'c
end
