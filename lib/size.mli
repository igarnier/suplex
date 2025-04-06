(** Vector sizes. *)

type _1 = private Size_1

type _2 = private Size_2

type _4 = private Size_4

type _8 = private Size_8

type _16 = private Size_16

type _32 = private Size_32

type _64 = private Size_64

type _128 = private Size_128

(** {2 Vector sizes} *)
type 'a t

val _1 : _1 t

val _2 : _2 t

val _4 : _4 t

val _8 : _8 t

val _16 : _16 t

val _32 : _32 t

val _64 : _64 t

val _128 : _128 t

val to_int : 'a t -> int

val to_int64 : 'a t -> int64

val equal : 'a t -> 'b t -> ('a, 'b) Type.eq option
