type _1 = Size_1

type _2 = Size_2

type _4 = Size_4

type _8 = Size_8

type _16 = Size_16

type _ t =
  | Size_1 : _1 t
  | Size_2 : _2 t
  | Size_4 : _4 t
  | Size_8 : _8 t
  | Size_16 : _16 t

let _1 = Size_1

let _2 = Size_2

let _4 = Size_4

let _8 = Size_8

let _16 = Size_16

let to_int : type a. a t -> int = function
  | Size_1 -> 1
  | Size_2 -> 2
  | Size_4 -> 4
  | Size_8 -> 8
  | Size_16 -> 16

let equal : type a b. a t -> b t -> (a, b) Type.eq option =
 fun sz1 sz2 ->
  match (sz1, sz2) with
  | (Size_1, Size_1) -> Some Type.Equal
  | (Size_2, Size_2) -> Some Type.Equal
  | (Size_4, Size_4) -> Some Type.Equal
  | (Size_8, Size_8) -> Some Type.Equal
  | (Size_16, Size_16) -> Some Type.Equal
  | _ -> None
