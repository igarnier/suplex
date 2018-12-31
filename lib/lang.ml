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


(* module type S =
 * sig
 * 
 *   type 'a repr
 * 
 *   module T : Type.S with type 'a repr = 'a repr
 * 
 *   open T
 * 
 *   (\* Atomic values *\)
 *   val unit : unit_t repr
 * 
 *   val int64  : int -> int64_t repr
 *   val int32  : int -> int32_t repr
 *   val int16  : int -> int16_t repr
 *   val int8   : int -> int8_t repr
 *   val float  : float -> float_t repr
 *   val double : float -> float_t repr
 * 
 *   val tt : bool_t repr
 *   val ff : bool_t repr
 * 
 *   (\* Operators on atomic values *\)
 *   val add : 'a numerical -> 'a repr -> 'a repr -> 'a repr
 *   val sub : 'a numerical -> 'a repr -> 'a repr -> 'a repr
 *   val mul : 'a numerical -> 'a repr -> 'a repr -> 'a repr
 *   val div : 'a numerical -> 'a repr -> 'a repr -> 'a repr
 * 
 *   val (&&) : bool_t repr -> bool_t repr -> bool_t repr
 *   val (||) : bool_t repr -> bool_t repr -> bool_t repr
 * 
 *   val lt : 'a numerical -> 'a repr -> 'a repr -> bool_t repr
 *   val le : 'a numerical -> 'a repr -> 'a repr -> bool_t repr
 *   val eq : 'a numerical -> 'a repr -> 'a repr -> bool_t repr
 * 
 *   (\* Printing atomic values *\)
 *   val print : 'a atomic -> 'a repr -> unit repr
 * 
 *   (\* Tuple introduction and elimination. Here, a proof of type 'a Repr shows
 *      that 'a is of the shape 'x1 repr * ('x2 repr * ... * unit). *\)
 *   val tuple : ReprTuple.t -> PureTuple.t repr
 *   val proj  : PureTuple.t repr -> 'a PureTuple.access -> 'a repr
 * 
 *   (\* Binding *\)
 *   val (>|=) : 'a repr -> ('a repr -> 'b repr) -> 'b repr
 * 
 *   (\* Conditionals *\)
 *   val cond : bool_t repr -> 'a repr -> 'a repr -> 'a repr
 * 
 *   (\*
 *      wit : 'a ReprTuble.t |- 'a = t1 repr * t2 repr * t3 repr
 *      ----
 *      res : (t1 * t2 * t3) repr
 *   *\)
 * 
 *   (\* val deref : 'a Type.ref repr -> 'a repr *\)
 * 
 *   (\* val new_array : 'a Type.atomic -> int repr -> 'a Type.array repr
 *    * 
 *    * val new_ref : 'a Type.atomic repr -> 'a Type.ref repr
 *    * 
 *    * val ( := ) : 'a Type.ref repr -> 'a Type.atomic -> unit repr
 *    * 
 *    * val ( .%() ) : 'a Type.array repr -> int repr -> 'a repr
 *    * 
 *    * val ( .%() <- ) : 'a array_typ repr -> 'a repr -> unit repr
 *    * 
 *    * val ( >>= ) : 'a repr -> ('a -> 'b repr) -> 'b repr
 *    * 
 *    * 
 *    * val while_loop : bool repr -> unit repr -> unit repr *\)
 * 
 * end *)
