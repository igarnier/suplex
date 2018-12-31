module type S =
sig
  type 'a repr

  module T : Type.S with type 'a repr = 'a repr

  open T

  val unit : unit_t repr
  val int64 : int -> int64_t repr
  val int32 : int -> int32_t repr
  val int16 : int -> int16_t repr
  val int8 : int -> int8_t repr
  val float : float -> float_t repr
  val double : float -> double_t repr
  val tt : bool_t repr
  val ff : bool_t repr
  val add : 'a numerical -> 'a repr -> 'a repr -> 'a repr
  val sub : 'a numerical -> 'a repr -> 'a repr -> 'a repr
  val mul : 'a numerical -> 'a repr -> 'a repr -> 'a repr
  val div : 'a numerical -> 'a repr -> 'a repr -> 'a repr
  val ( && ) : bool_t repr -> bool_t repr -> bool_t repr
  val ( || ) : bool_t repr -> bool_t repr -> bool_t repr
  val lt : 'a numerical -> 'a repr -> 'a repr -> bool_t repr
  val le : 'a numerical -> 'a repr -> 'a repr -> bool_t repr
  val eq : 'a numerical -> 'a repr -> 'a repr -> bool_t repr
  val print : 'a repr -> unit_t repr
  val tuple : ex_repr list -> tuple_t repr
  val proj  : tuple_t repr -> 'a access -> 'a repr
  val ( >|= ) : 'a repr -> ('a repr -> 'b repr) -> 'b repr
  val cond : bool_t repr -> 'a repr -> 'a repr -> 'a repr
end

