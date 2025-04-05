module Size : sig
  type _1 = private Size_1

  type _2 = private Size_2

  type _4 = private Size_4

  type _8 = private Size_8

  type _16 = private Size_16

  type 'a t

  val _1 : _1 t

  val _2 : _2 t

  val _4 : _4 t

  val _8 : _8 t

  val _16 : _16 t

  val to_int : 'a t -> int

  val equal : 'a t -> 'b t -> ('a, 'b) Type.eq option
end

type i64 = private I64

type i32 = private I32

type i16 = private I16

type i8 = private I8

type i1 = private I1

type f32 = private F32

type f64 = private F64

type ('a, 'sz) vec = private Vector

type 'a base_numerical =
  | I64_num : i64 base_numerical
  | I32_num : i32 base_numerical
  | I16_num : i16 base_numerical
  | I8_num : i8 base_numerical
  | I1_num : i1 base_numerical
  | F64_num : f64 base_numerical
  | F32_num : f32 base_numerical

type 'a numerical =
  | Base_num : 'a base_numerical -> 'a numerical
  | Vec_num :
      { base : 'a base_numerical; numel : 'sz Size.t }
      -> ('a, 'sz) vec numerical

type 'a record

type ('a, 'b, 'c, 'd) record_desc

type 'a ptr = private Ptr

type ('a, 'k) arr = private Arr

type 'a fn

type 'a typ = private
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

and ('a, 't) field = private
  | Field : { name : string; ty : 'a typ } -> ('a, 't) field

type 'a stack

type 'a stack_var

type 'a expr

type 'a fundecl

type 'a intrinsic

type ('a, 'b) args

type ('s, 'o) num_rel

type 'sz mask

module Types : sig
  type nonrec 'a typ = 'a typ

  val field_name : ('a, 'b) field -> string

  val field_type : ('a, 'b) field -> 'a typ

  val pp_fn : int list -> Format.formatter -> 'a fn -> unit

  val pp_typ : Format.formatter -> 'a typ -> unit

  val pp_base_numerical : Format.formatter -> 'a base_numerical -> unit

  val pp_field : Format.formatter -> ('a, 'b) field -> unit

  val type_eq : 'a typ -> 'b typ -> bool

  val record_eq :
    ('a, 'b, 'c, 'u) record_desc -> ('d, 'e, 'f, 'v) record_desc -> bool

  val field_eq : ('a, 'b) field -> ('c, 'd) field -> bool

  val field_index : ('a, 'u) field -> 'u typ -> (int option, string) Result.t

  val base_num : 'a base_numerical -> 'a typ

  val num : 'a numerical -> 'a typ

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

  val vec : 'a base_numerical -> 'b Size.t -> ('a, 'b) vec typ

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

  (** Witness that [t] is related to OCaml constants of type [v]. *)
  val rel : (t, v) num_rel

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

val make_i64_vec :
  'dim Size.t ->
  (module Numerical with type t = (i64, 'dim) vec and type v = int64 array)

val make_i32_vec :
  'dim Size.t ->
  (module Numerical with type t = (i32, 'dim) vec and type v = int32 array)

val make_i16_vec :
  'dim Size.t ->
  (module Numerical with type t = (i16, 'dim) vec and type v = int array)

val make_i8_vec :
  'dim Size.t ->
  (module Numerical with type t = (i8, 'dim) vec and type v = int array)

val make_i1_vec :
  'dim Size.t ->
  (module Numerical with type t = (i1, 'dim) vec and type v = bool array)

val make_f64_vec :
  'dim Size.t ->
  (module Numerical with type t = (f64, 'dim) vec and type v = float array)

val make_f32_vec :
  'dim Size.t ->
  (module Numerical with type t = (f32, 'dim) vec and type v = float array)

module I64_ba : BA with type elt = i64

module I32_ba : BA with type elt = i32

module I16_ba : BA with type elt = i16

module I8_ba : BA with type elt = i8

module F64_ba : BA with type elt = f64

module F32_ba : BA with type elt = f32

val i64_num : I64.t base_numerical

val i32_num : I32.t base_numerical

val i16_num : I16.t base_numerical

val i8_num : I8.t base_numerical

val f64_num : F64.t base_numerical

val f32_num : F32.t base_numerical

val scalar : 'a base_numerical -> 'a typ

val vector : 'a base_numerical -> 'sz Size.t -> ('a, 'sz) vec typ

val base_numerical_kind : 'a base_numerical -> [ `fp | `int ]

val ( @-> ) : 'a typ -> 'b fn -> ('a expr -> 'b) fn

val returning : 'a typ -> 'a expr fn

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

(** [cast arr ty] casts the element type of an array to a target type. It is up
    to the user to ensure that the cast is sensible. *)
val cast : ('a, [ `unk ]) arr expr -> 'b typ -> ('b, [ `unk ]) arr expr

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

(** [arg e vec] pushes [e] on the vector of arguments [vec] *)
val arg : 'a expr -> ('b, 'c) args -> ('a expr -> 'b, 'c) args

(** [empty_args] is the empty vector of arguments *)
val empty_args : ('a expr, 'a expr) args

(** [addr_of_arr arr] is the address of the array [arr] *)
val addr_of_arr : ('a, [ `cst ]) arr expr -> ('a, [ `cst ]) arr ptr expr

(** [addr_of_rec rec] is the address of the record [rec] *)
val addr_of_rec : 'a record expr -> 'a record ptr expr

(** [ptr_eq p1 p2] is true if the pointers [p1] and [p2] are equal *)
val ptr_eq : 'a ptr expr -> 'a ptr expr -> bool expr

(** [fail msg] raises a [Failure] exception with message [msg] *)
val fail : string -> 'a expr

(** [malloc ty] allocates a block of memory of type [ty] *)
val malloc : 'a typ -> 'a ptr expr

(** [malloc_array ty n] allocates a block of memory with [n] elements of type
    [ty] *)
val malloc_array : 'a typ -> i64 expr -> ('a, [ `unk ]) arr expr

(** [free p] frees the memory block pointed to by [p] *)
val free : 'a ptr expr -> unit expr

(** [free_array arr] frees the memory block pointed to by [arr] *)
val free_array : ('a, [ `unk ]) arr expr -> unit expr

(** [trunc n1 n2] constructs a integer truncating cast from [n1] to [n2].
    Returns [None] if a truncation doesn't exist, e.g. if [n1] is smaller than
    [n2]. *)
val trunc : 'a numerical -> 'b numerical -> 'a expr -> 'b expr

(** [sext n1 n2] constructs a sign-extending cast from [n1] to [n2]. *)
val sext : 'a numerical -> 'b numerical -> 'a expr -> 'b expr

(** [zext n1 n2] constructs a zero-extending cast from [n1] to [n2]. *)
val zext : 'a numerical -> 'b numerical -> 'a expr -> 'b expr

(** [to_f32 n e] constructs a cast of the expression [e] from [n] to [f32]. *)
val to_f32 : 'a base_numerical -> 'a expr -> f32 expr

(** [to_f64 n e] constructs a cast of the expression [e] from [n] to [f64]. *)
val to_f64 : 'a base_numerical -> 'a expr -> f64 expr

(** [of_f32 n e] constructs a cast of the expression [e] from [f32] to [n]. *)
val of_f32 : 'a base_numerical -> f32 expr -> 'a expr

(** [of_f64 n e] constructs a cast of the expression [e] from [f64] to [n]. *)
val of_f64 : 'a base_numerical -> f64 expr -> 'a expr

(** [vec_to_f32 n e] constructs a cast of the expression [e] from [n] to [f32].
*)
val vec_to_f32 : 'a base_numerical -> ('a, 'sz) vec expr -> (f32, 'sz) vec expr

(** [vec_to_f64 n e] constructs a cast of the expression [e] from [n] to [f64].
*)
val vec_to_f64 : 'a base_numerical -> ('a, 'sz) vec expr -> (f64, 'sz) vec expr

(** [vec_of_f32 n e] constructs a cast of the expression [e] from [f32] to [n].
*)
val vec_of_f32 : 'a base_numerical -> (f32, 'sz) vec expr -> ('a, 'sz) vec expr

(** [vec_of_f64 n e] constructs a cast of the expression [e] from [f64] to [n].
*)
val vec_of_f64 : 'a base_numerical -> (f64, 'sz) vec expr -> ('a, 'sz) vec expr

(** [mask sz array] creates a mask with prescribed size.

    @raise Invalid_argument if [Size.to_int sz <> Array.length array] *)
val mask : 'sz Size.t -> int array -> 'sz mask

val shuffle :
  ('a, _) vec expr -> ('a, _) vec expr -> 'sz mask -> ('a, 'sz) vec expr

(** Undefined value of a given type. *)
val undef : 'a typ -> 'a expr

(** [block stmts] is a block of statements [stmts]. *)
val block : unit expr list -> unit expr

(** The module [Stack] provoides primitives for describing stack frames. *)
module Stack : sig
  (** [end_frame body] marks the end of the stack frame declaration and takes as
      argument the body of the function. *)
  val end_frame : 'a -> 'a stack

  (** [stack_var @+ rest] declares a stack variable and continues with the
      [rest] of the stack frame. *)
  val ( @+ ) : 'a stack_var -> ('a expr -> 'b stack) -> 'b stack

  (** [unit] declares a stack variable of type [unit] *)
  val unit : unit ptr stack_var

  (** [bool] declares a stack variable of type [bool] *)
  val bool : bool ptr stack_var

  (** [num n] declares a stack variable of type [TNum n] *)
  val num : 'a numerical -> 'a ptr stack_var

  (** [i64] declares a stack variable of type [i64] *)
  val i64 : i64 ptr stack_var

  (** [i32] declares a stack variable of type [i32] *)
  val i32 : i32 ptr stack_var

  (** [i16] declares a stack variable of type [i16] *)
  val i16 : i16 ptr stack_var

  (** [i8] declares a stack variable of type [i8] *)
  val i8 : i8 ptr stack_var

  (** [f64] declares a stack variable of type [f64] *)
  val f64 : f64 ptr stack_var

  (** [f32] declares a stack variable of type [f32] *)
  val f32 : f32 ptr stack_var

  (** [ptr ty] declares a stack variable of type [ptr] *)
  val ptr : 'a typ -> 'a ptr ptr stack_var

  (** [arr ty n] declares an array stack variable. *)
  val arr : 'a typ -> i64 expr -> ('a, [ `unk ]) arr stack_var

  (** [arr_cst ty n] declares a fixed-size array stack variable. *)
  val arr_cst : 'a typ -> int64 -> ('a, [ `cst ]) arr stack_var

  (** [vec ty sz] declares a vector stack variable. *)
  val vec : 'a base_numerical -> 'sz Size.t -> ('a, 'sz) vec ptr stack_var

  (** [strct desc] declares a record stack variable. *)
  val strct :
    ('a, 'b Vec.t, 'b Vec.t, 'c record) record_desc -> 'c record stack_var
end

(** Alias for [Stack.(@+)] *)
val local : 'a stack_var -> ('a expr -> 'b stack) -> 'b stack

(** Alias for [Stack.(@+)] *)
val ( let*:: ) : 'a stack_var -> ('a expr -> 'b stack) -> 'b stack

(** Alias for [Stack.end_frame] *)
val end_frame : 'a -> 'a stack

(** [fundecl name proto def] defines a function with name [name], type signature
    [proto] and body [def]. Functions can be nested but it is a programming
    error to refer to the local variables from an outer function. *)
val fundecl : string -> 'a fn -> ('a fn expr -> 'a) stack -> 'a fn expr

(** [intrinsic name proto] defines an intrinsic function with name [name] and
    type signature [proto]. *)
val intrinsic : string -> 'a fn -> 'a intrinsic

(** The [Run] module allows to call functions defined using this library from
    OCaml. The entrypoint is a ['a module_] which is a list of function
    definitions. Each function definition must specify its public interface,
    which corresponds to a relation between Suplex types and OCaml types. *)
module Run : sig
  (** The type of arguments for functions callable from OCaml. The first type
      parameters encodes the type of the argument for the suplex implementation
      while the second type argument encodes the type of the values passed from
      OCaml. *)
  type ('suplex, 'ocaml) rel

  (** The type of vector of arguments for functions callable from OCaml. *)
  type ('suplex, 'ocaml) rel_vec

  (** The type of functions callable from OCaml. *)
  type ('suplex, 'ocaml) fn_rel

  (** The type of opaque structures. Used to pass ctypes struct to suplex
      functions. *)
  type 'a opaque

  (** The type of suplex modules. A suplex modules is a vector of function
      definitions, including a {!amain} one. *)
  type 'a module_

  (** The empty vector of arguments. *)
  val empty : (unit Vec.t, unit Vec.t) rel_vec

  (** [|+] prepends an argument to a vector of arguments. *)
  val ( |+ ) :
    ('a Vec.t, 'b Vec.t) rel_vec ->
    ('c expr, 'd) rel ->
    (('c * 'a) Vec.t, ('d * 'b) Vec.t) rel_vec

  (** Arguments of type [unit] *)
  val unit : (unit expr, unit) rel

  (** Arguments of type [bool] *)
  val bool : (bool expr, bool) rel

  (** Arguments of type [i64], passed as [int64] values. *)
  val i64 : (i64 expr, int64) rel

  (** Arguments of type [i32], passed as [int32] values. *)
  val i32 : (i32 expr, int32) rel

  (** Arguments of type [i16], passed as [int] values. *)
  val i16 : (i16 expr, int) rel

  (** Arguments of type [i8], passed as [int] values. *)
  val i8 : (i8 expr, int) rel

  (** Arguments of type [i1], passed as [bool] values. *)
  val i1 : (i1 expr, bool) rel

  (** Arguments of type [f64], passed as [float] values. *)
  val f64 : (f64 expr, float) rel

  (** Arguments of type [f32], passed as [float] values. *)
  val f32 : (f32 expr, float) rel

  (** Arguments of type [I64_ba.s record], passed as bigarrays containing
      [int64] elements in C layout. *)
  val bigarray_i64 :
    ( I64_ba.s record expr,
      (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t )
    rel

  (** See {!bigarray_i64}. *)
  val bigarray_i32 :
    ( I32_ba.s record expr,
      (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t )
    rel

  (** See {!bigarray_i64}. *)
  val bigarray_i16 :
    ( I16_ba.s record expr,
      (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t )
    rel

  (** See {!bigarray_i64}. *)
  val bigarray_i8 :
    ( I8_ba.s record expr,
      (int, Bigarray.int8_signed_elt, Bigarray.c_layout) Bigarray.Array1.t )
    rel

  (** See {!bigarray_i64}. *)
  val bigarray_f64 :
    ( F64_ba.s record expr,
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t )
    rel

  (** See {!bigarray_i64}. *)
  val bigarray_f32 :
    ( F32_ba.s record expr,
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t )
    rel

  (** Pointer to bytes arrays, passed as string values. *)
  val string : (i8 ptr expr, string) rel

  (** Array of statically unknown size, passed as [Seq.t] iterators. *)
  val array_raw : ('a expr, 'b) rel -> (('a, [ `unk ]) arr expr, 'b Seq.t) rel

  (** Array of statically known size, passed as [Seq.t] iterators. *)
  val array :
    int -> ('a expr, 'b) rel -> (('a, [ `cst ]) arr expr, 'b Seq.t) rel

  (** Specify the relation between an open record and a vector of types. Used
      together with {!mallocd_strct} *)
  val strct :
    ('a, 'b Vec.t, 'b Vec.t, 'c record) record_desc ->
    ('b Vec.t, 'd Vec.t) rel_vec ->
    ('c record expr, 'd Vec.t) rel

  (** Open records, passed as vector of values used as initializer for a
      malloc'd ctypes struct. *)
  val mallocd_strct :
    ('a, 'b Vec.t, 'b Vec.t, 'c record) record_desc ->
    ('b Vec.t, 'd Vec.t) rel_vec ->
    ('c record expr, 'd Vec.t) rel

  (** Sealed records, passed as pointers to ctypes structs. *)
  val opaque_mallocd_strct :
    ('a, 'b Vec.t, 'b Vec.t, 'c record) record_desc ->
    ('c record expr, 'c opaque) rel

  val returning : ('a expr, 'b) rel -> ('a expr, 'b) fn_rel

  val ( @-> ) :
    ('a expr, 'b) rel -> ('c, 'd) fn_rel -> ('a expr -> 'c, 'b -> 'd) fn_rel

  val add_intrinsic : 'a intrinsic -> ('a fn expr -> 'b module_) -> 'b module_

  val add_fundecl :
    string ->
    ('a, 'b -> 'c) fn_rel ->
    ('a fn expr -> 'a) stack ->
    ('a fn expr -> 'd module_) ->
    ('d * ('b -> 'c)) module_

  (** [main name rel def] is the main function of a module.
      { ul
      {- [name] is the name of the function}
      {- [rel] specifies the signature of the function}
      {- [def] is the body of the function}
      }
  *)
  val main :
    string ->
    ('a, 'b -> 'c) fn_rel ->
    ('a fn expr -> 'a) stack ->
    ('b -> 'c) module_

  (** [jit_module ?debug ?cfg ?state mdl] compiles [mdl] and returns a vector of
      callable functions. *)
  val jit_module :
    ?debug:bool ->
    ?cfg:Llvm_executionengine.llcompileroptions ->
    ?state:Compile.llvm_state ->
    'a module_ ->
    'a

  (** [jit_program ?cfg ?state rel def] compiles [decl] into a function with
      type specified by [rel]. *)
  val jit_program :
    ?debug:bool ->
    ?cfg:Llvm_executionengine.llcompileroptions ->
    ?state:Compile.llvm_state ->
    ('a, 'b -> 'c) fn_rel ->
    'a fundecl ->
    'b ->
    'c

  (** [jit ?cfg ?fname rel def] compiles the function specified by [def] with
      type specified by [rel]. *)
  val jit :
    ?debug:bool ->
    ?cfg:Llvm_executionengine.llcompileroptions ->
    ?fname:string ->
    ('a, 'b -> 'c) fn_rel ->
    ('a fn expr -> 'a) stack ->
    'b ->
    'c
end
