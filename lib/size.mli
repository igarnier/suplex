(** Vector sizes. *)

type _1 = private Size_1

type _2 = private Size_2

type _4 = private Size_4

type _8 = private Size_8

type _16 = private Size_16

(** {2 Vector sizes} *)
type 'a t

val _1 : _1 t

val _2 : _2 t

val _4 : _4 t

val _8 : _8 t

val _16 : _16 t

val to_int : 'a t -> int

val equal : 'a t -> 'b t -> ('a, 'b) Type.eq option
