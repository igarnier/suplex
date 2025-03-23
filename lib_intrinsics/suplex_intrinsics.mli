open Suplex

module type Float_intrinsics = sig
  type t

  val sqrt : (t expr -> t expr) intrinsic

  val powi : (t expr -> i32 expr -> t expr) intrinsic

  val sin : (t expr -> t expr) intrinsic

  val cos : (t expr -> t expr) intrinsic

  val tan : (t expr -> t expr) intrinsic

  val asin : (t expr -> t expr) intrinsic

  val acos : (t expr -> t expr) intrinsic

  val atan : (t expr -> t expr) intrinsic

  val atan2 : (t expr -> t expr -> t expr) intrinsic

  val sinh : (t expr -> t expr) intrinsic

  val cosh : (t expr -> t expr) intrinsic

  val tanh : (t expr -> t expr) intrinsic

  val pow : (t expr -> t expr -> t expr) intrinsic

  val exp : (t expr -> t expr) intrinsic

  val exp2 : (t expr -> t expr) intrinsic

  val exp10 : (t expr -> t expr) intrinsic

  val log : (t expr -> t expr) intrinsic

  val log10 : (t expr -> t expr) intrinsic

  val fma : (t expr -> t expr -> t expr -> t expr) intrinsic

  val log2 : (t expr -> t expr) intrinsic
end

module F64 : sig
  include module type of F64

  include Float_intrinsics with type t := f64
end

module F32 : sig
  include module type of F32

  include Float_intrinsics with type t := f32
end

module Vector : sig
  module Reduce : sig
    (** [op] describes reduction operations. *)
    type op

    (** [add] is the integer addition operation. *)
    val add : op

    (** [mul] is the integer multiplication operation. *)
    val mul : op

    (** [fadd] is the floating-point addition operation. *)
    val fadd : op

    (** [fsub] is the floating-point subtraction operation. *)
    val fmul : op

    (** [and_] is the bitwise AND operation. *)
    val and_ : op

    (** [or_] is the bitwise OR operation. *)
    val or_ : op

    (** [xor] is the bitwise XOR operation. *)
    val xor : op

    (** [smax] is the signed maximum operation. *)
    val smax : op

    (** [smin] is the signed minimum operation. *)
    val smin : op

    (** [umax] is the unsigned maximum operation. *)
    val umax : op

    (** [umin] is the unsigned minimum operation. *)
    val umin : op

    (** [fmax] is the floating-point maximum operation. *)
    val fmax : op

    (** [fmin] is the floating-point minimum operation. *)
    val fmin : op

    (** [fmaximum] is the floating-point maximum operation, and propagates NaNs.
    *)
    val fmaximum : op

    (** [fminimum] is the floating-point minimum operation, and propagates NaNs.
    *)
    val fminimum : op

    (** [string_of_op op] is a string representation of the operation [op]. *)
    val string_of_op : op -> string

    (** [reduce op numty len] is a vector reduction intrinsic for the operation
        [op]. Note that not all reduction operations work on all types.

        @raise Failure if the operation is not supported for the given type. *)
    val reduce :
      op ->
      's base_numerical ->
      'len Size.t ->
      (('s, 'len) vec expr -> 's expr) intrinsic

    (** [reduce_acc op numty len] is a vector reduction intrinsic for the
        operation [op]. Note that not all reduction operations work on all
        types.

        @raise Failure if the operation is not supported for the given type. *)
    val reduce_acc :
      op ->
      's base_numerical ->
      'len Size.t ->
      ('s expr -> ('s, 'len) vec expr -> 's expr) intrinsic
  end

  (* type 'sz mask = (i1, 'sz) vec expr *)

  (* val vp_select : *)
  (*   's base_numerical -> *)
  (*   'len Size.t -> *)
  (*   ('len mask -> *)
  (*   ('s, 'len) vec expr -> *)
  (*   ('s, 'len) vec expr -> *)
  (*   i32 expr -> *)
  (*   ('s, 'len) vec expr) *)
  (*   intrinsic *)

  (* val vp_merge : *)
  (*   's base_numerical -> *)
  (*   'len Size.t -> *)
  (*   ('len mask -> *)
  (*   ('s, 'len) vec expr -> *)
  (*   ('s, 'len) vec expr -> *)
  (*   ('s, 'len) vec expr) *)
  (*   intrinsic *)

  (* val vp_unop : *)
  (*   string -> *)
  (*   's base_numerical -> *)
  (*   'len Size.t -> *)
  (*   (('s, 'len) vec expr -> *)
  (*   i1 expr -> *)
  (*   'len mask -> *)
  (*   i32 expr -> *)
  (*   ('s, 'len) vec expr) *)
  (*   intrinsic *)

  (* val vp_binop : *)
  (*   string -> *)
  (*   'a base_numerical -> *)
  (*   'len Size.t -> *)
  (*   (('a, 'len) vec expr -> *)
  (*   ('a, 'len) vec expr -> *)
  (*   'len mask -> *)
  (*   i32 expr -> *)
  (*   ('a, 'len) vec expr) *)
  (*   intrinsic *)
end
