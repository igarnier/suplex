open Suplex

module Vector : sig
  module Reduce : sig
    (** [op] describes reduction operations. *)
    type op

    val add : op

    val mul : op

    val fadd : op

    val fmul : op

    val and_ : op

    val or_ : op

    val xor : op

    val smax : op

    val smin : op

    val umax : op

    val umin : op

    val fmax : op

    val fmin : op

    val fmaximum : op

    val fminimum : op

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

  type 'sz mask = (bool, 'sz) vec expr

  val vp_select :
    's base_numerical ->
    'len Size.t ->
    ('len mask ->
    ('s, 'len) vec expr ->
    ('s, 'len) vec expr ->
    i32 expr ->
    ('s, 'len) vec expr)
    intrinsic

  val vp_merge :
    's base_numerical ->
    'len Size.t ->
    ('len mask ->
    ('s, 'len) vec expr ->
    ('s, 'len) vec expr ->
    ('s, 'len) vec expr)
    intrinsic

  val vp_unop :
    string ->
    's base_numerical ->
    'len Size.t ->
    (('s, 'len) vec expr ->
    bool expr ->
    'len mask ->
    i32 expr ->
    ('s, 'len) vec expr)
    intrinsic

  val vp_binop :
    string ->
    'a base_numerical ->
    'len Size.t ->
    (('a, 'len) vec expr ->
    ('a, 'len) vec expr ->
    'len mask ->
    i32 expr ->
    ('a, 'len) vec expr)
    intrinsic
end
