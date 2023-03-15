type 'a vec = 'a Vec.t

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

(** ['a numerical] is the type of all {b suplex} numerical types. *)
type 'a numerical =
  | I64_num : i64 numerical
  | I32_num : i32 numerical
  | I16_num : i16 numerical
  | I8_num : i8 numerical
  | F32_num : f32 numerical
  | F64_num : f64 numerical

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

(** Classifies numerical types into floating-point kind [`fp] or integer kind [`int]. *)
let numerical_kind : type a. a numerical -> [ `fp | `int ] =
 fun n ->
  match n with
  | I64_num -> `int
  | I32_num -> `int
  | I16_num -> `int
  | I8_num -> `int
  | F32_num -> `fp
  | F64_num -> `fp

type ('a, 'b) refl = Eq : ('a, 'a) refl

let numerical_eq : type a b. a numerical -> b numerical -> (a, b) refl option =
 fun n1 n2 ->
  match (n1, n2) with
  | (I64_num, I64_num) -> Some Eq
  | (I32_num, I32_num) -> Some Eq
  | (I16_num, I16_num) -> Some Eq
  | (I8_num, I8_num) -> Some Eq
  | (F64_num, F64_num) -> Some Eq
  | (F32_num, F32_num) -> Some Eq
  | _ -> None

(** The module type describing the {b suplex} type system. *)
module type S = sig
  (** [('a, 'a_typ) typed_term] is the type of typed terms. See the definition of ['a m] below. *)
  type (!'a, 'a_typ) typed_term

  (** ['a ptr] is the abstract type of pointers. *)
  type !'a ptr

  (** [('a, 'c) arr] is the abstract type of arrays. The first type parameter
      is the type of elements while the second type parameter encodes whether
      the array has a statically known size or not. *)
  type (!'a, 'c) arr

  type ex_numerical = Ex_num : 'a numerical -> ex_numerical

  (** ['a typ] is the type of {b suplex} types. *)
  type 'a typ =
    | TUnit : unit typ
    | TBool : bool typ
    | TNum : 'a numerical -> 'a typ
    | TPtr : 'a typ -> 'a ptr typ
    | TArr_unk : 'a typ -> ('a, [ `unk ]) arr typ
    | TArr_cst : 'a typ * int64 -> ('a, [ `cst ]) arr typ
    | TRecord : (_, 'u vec, 'u vec, 't) record -> 't typ

  (** [('elim, 't_acc, 't, 'u) record] is the type of record descriptors.
      The type parameters are as follows:
      - ['elim] is the type of eliminators for this record type
      - ['t_acc] is a type-level accumulator used internally, you can ignore it
      - ['t] is the type of elements held in the record
      - ['u] is the final type of the record, which will be visible after {!seal} is called. *)
  and ('elim, 't_acc, 't, 'u) record =
    | Record_empty : (unit, unit vec, 't vec, 'u) record
    | Record_field :
        ('a, 'u) field * ('e, 't_acc vec, 't vec, 'u) record
        -> ('e * ('a, 'u) proj, ('a * 't_acc) vec, 't vec, 'u) record
    | Record_fix :
        int * ('u typ -> ('b, 'd vec, 'd vec, 'u) record)
        -> ('b, 'd vec, 'd vec, 'u) record

  (** The type of record fields. *)
  and ('a, 't) field =
    | Field : { name : string; ty : 'a typ } -> ('a, 't) field

  (** The type of record projections. *)
  and ('a, 'u) proj =
    | Proj :
        { get : 'u ptr m -> 'a m;
          getaddr : 'u ptr m -> 'a ptr m;
          set : 'u ptr m -> 'a m -> unit m
        }
        -> ('a, 'u) proj

  (** The type of typed terms. *)
  and !'a m = ('a, 'a typ) typed_term

  (** [type_eq ty1 ty2] checks type equality. Type equality is structural except
      for records (i.e. two identical record types are not equal). *)
  val type_eq : 'a typ -> 'b typ -> bool

  (** [field_name field] returns the name of a field. *)
  val field_name : ('a, 'b) field -> string

  (** [field_type field] returns the type of a field. *)
  val field_type : ('a, 'b) field -> 'a typ

  (** [pp_typ] pretty prints types. *)
  val pp_typ : Format.formatter -> 'a typ -> unit

  (** The [unit] type. *)
  val unit : unit typ

  (** The type of booleans. *)
  val bool : bool typ

  (** The type of signed 64 bits integers *)
  val i64 : i64 typ

  (** The type of signed 32 bits integers *)
  val i32 : i32 typ

  (** The type of signed 16 bits integers *)
  val i16 : i16 typ

  (** The type of signed 8 bits integers *)
  val i8 : i8 typ

  (** The type of single-precision floating-point numbers *)
  val f32 : f32 typ

  (** The type of double-precision floating-point numbers *)
  val f64 : f64 typ

  (** [ptr typ] is the type of pointers to data of type [typ]. *)
  val ptr : 'a typ -> 'a ptr typ

  (** The type of arrays holding values of type [typ], with unknown length. *)
  val arr : 'a typ -> ('a, [ `unk ]) arr typ

  (** The type of arrays holding values of type [typ], with statically known length.
      These arrays are stored packed in structures. *)
  val arr_cst : 'a typ -> int64 -> ('a, [ `cst ]) arr typ

  (** [empty_rec] is the empty record. *)
  val empty_rec : (unit, unit vec, 't vec, 'u) record

  (** [field name ty] is a field [name] holding data of type [ty]. *)
  val field : string -> 'a typ -> ('a, 'u) field

  (** [record |+ field] extends [record] with [field]. *)
  val ( |+ ) :
    ('b, 'c vec, 'd vec, 'e) record ->
    ('f, 'e) field ->
    ('b * ('f, 'e) proj, ('f * 'c) vec, 'd vec, 'e) record

  (** [fix record] allows to have recursive record types. *)
  val fix :
    ('u typ -> ('b, 'd vec, 'd vec, 'u) record) ->
    ('b, 'd vec, 'd vec, 'u) record

  (** [seal record] packs a record in a regular type. *)
  val seal : ('b, 'd vec, 'd vec, 'u) record -> 'u typ

  (** ['a ] fn is the type of functions with prototype ['a]. *)
  type _ fn =
    | Returning : 'a typ -> 'a m fn
    | Arrow : 'a typ * 'b fn -> ('a m -> 'b) fn

  (** [fn_eq] test function type equality. *)
  val fn_eq : 'a fn -> 'b fn -> bool

  (** [returning ty] constructs the type of a function returning a value of type [ty]. *)
  val returning : 'a typ -> 'a m fn

  (** [ty @-> fn] extends the function type [fn] to take an argument of type [ty]. *)
  val ( @-> ) : 'a typ -> 'b fn -> ('a m -> 'b) fn
end

module Make (M : sig
  type (!'a, 'a_typ) typed_term

  type !'a ptr

  type (!'a, 'c) arr
end) :
  S
    with type ('a, 'b) typed_term = ('a, 'b) M.typed_term
     and type 'a ptr = 'a M.ptr
     and type ('a, 'c) arr = ('a, 'c) M.arr = struct
  type ('a, 'b) typed_term = ('a, 'b) M.typed_term

  type 'a ptr = 'a M.ptr

  type ('a, 'c) arr = ('a, 'c) M.arr

  type ex_numerical = Ex_num : 'a numerical -> ex_numerical

  type 'a typ =
    | TUnit : unit typ
    | TBool : bool typ
    | TNum : 'a numerical -> 'a typ
    | TPtr : 'a typ -> 'a ptr typ
    | TArr_unk : 'a typ -> ('a, [ `unk ]) arr typ
    | TArr_cst : 'a typ * int64 -> ('a, [ `cst ]) arr typ
    | TRecord : (_, 'u vec, 'u vec, 't) record -> 't typ

  and ('elim, 't_acc, 't, 'u) record =
    | Record_empty : (unit, unit vec, 't vec, 'u) record
    | Record_field :
        ('a, 'u) field * ('e, 't_acc vec, 't vec, 'u) record
        -> ('e * ('a, 'u) proj, ('a * 't_acc) vec, 't vec, 'u) record
    | Record_fix :
        int * ('u typ -> ('b, 'd vec, 'd vec, 'u) record)
        -> ('b, 'd vec, 'd vec, 'u) record

  and ('a, 't) field =
    | Field : { name : string; ty : 'a typ } -> ('a, 't) field

  and ('a, 'u) proj =
    | Proj :
        { get : 'u ptr m -> 'a m;
          getaddr : 'u ptr m -> 'a ptr m;
          set : 'u ptr m -> 'a m -> unit m
        }
        -> ('a, 'u) proj

  and !'a m = ('a, 'a typ) typed_term

  let field_name : type a. (a, _) field -> string = function
    | Field { name; _ } -> name

  let field_type : type a. (a, _) field -> a typ = function
    | Field { ty; _ } -> ty

  let rec pp_typ : type a. int list -> Format.formatter -> a typ -> unit =
    fun (type a) visited fmtr (typ : a typ) ->
     match typ with
     | TUnit -> Format.pp_print_string fmtr "unit"
     | TBool -> Format.pp_print_string fmtr "bool"
     | TNum n -> pp_numerical fmtr n
     | TPtr t -> Format.fprintf fmtr "[%a]" (pp_typ visited) t
     | TArr_unk t -> Format.fprintf fmtr "<%a>" (pp_typ visited) t
     | TArr_cst (t, sz) -> Format.fprintf fmtr "<%a:%Ld>" (pp_typ visited) t sz
     | TRecord descr ->
         let rec loop :
             type x y z.
             int list -> Format.formatter -> (x, y, z, a) record -> unit =
          fun visited fmtr descr ->
           match descr with
           | Record_empty -> ()
           | Record_field (field, rest) ->
               Format.fprintf
                 fmtr
                 "%s: %a"
                 (field_name field)
                 (pp_typ visited)
                 (field_type field) ;
               Format.fprintf fmtr ";@," ;
               loop visited fmtr rest
           | Record_fix (id, f) ->
               if List.mem id visited then Format.fprintf fmtr "#%d" id
               else
                 Format.fprintf
                   fmtr
                   "fix %d. %a"
                   id
                   (loop (id :: visited))
                   (f typ)
         in
         Format.fprintf fmtr "@[{" ;
         loop visited fmtr descr ;
         Format.fprintf fmtr "}@]"

  let pp_typ fmtr typ = pp_typ [] fmtr typ

  let rec type_eq : type a b. a typ -> b typ -> bool =
   fun t1 t2 ->
    match (t1, t2) with
    | (TUnit, TUnit) -> true
    | (TBool, TBool) -> true
    | (TNum n1, TNum n2) -> (
        match numerical_eq n1 n2 with None -> false | Some Eq -> true)
    | (TPtr t1, TPtr t2) -> type_eq t1 t2
    | (TArr_unk t1, TArr_unk t2) -> type_eq t1 t2
    | (TArr_cst (t1, sz1), TArr_cst (t2, sz2)) -> sz1 = sz2 && type_eq t1 t2
    | (TRecord descr1, TRecord descr2) -> record_eq descr1 descr2
    | _ -> false

  and record_eq :
      type a b c u d e f v. (a, b, c, u) record -> (d, e, f, v) record -> bool =
   fun descr1 descr2 ->
    match (descr1, descr2) with
    | (Record_empty, Record_empty) -> true
    | (Record_field (f1, r1), Record_field (f2, r2)) ->
        field_eq f1 f2 && record_eq r1 r2
    | (Record_fix (id1, _), Record_fix (id2, _)) -> id1 = id2
    | _ -> false

  and field_eq : type a b c d. (a, b) field -> (c, d) field -> bool =
   fun field1 field2 ->
    match (field1, field2) with
    | (Field field1, Field field2) ->
        String.equal field1.name field2.name && type_eq field1.ty field2.ty

  let unit = TUnit

  let bool = TBool

  let i64 = TNum I64_num

  let i32 = TNum I32_num

  let i16 = TNum I16_num

  let i8 = TNum I8_num

  let f32 = TNum F32_num

  let f64 = TNum F64_num

  let ptr x = TPtr x

  let arr typ = TArr_unk typ

  let arr_cst ty sz =
    if sz < 0L then invalid_arg "Type_system.vec: negative static size" ;
    TArr_cst (ty, sz)

  let empty_rec = Record_empty

  let field : type a. string -> a typ -> (a, _) field =
   fun name ty -> Field { name; ty }

  let ( |+ ) rest field = Record_field (field, rest)

  let gensym =
    let struct_id = ref 0 in
    fun () ->
      let id = !struct_id in
      incr struct_id ;
      id

  let fix f =
    let id = gensym () in
    Record_fix (id, f)

  let seal descr = TRecord descr

  type _ fn =
    | Returning : 'a typ -> 'a m fn
    | Arrow : 'a typ * 'b fn -> ('a m -> 'b) fn

  let returning ty = Returning ty

  let ( @-> ) ty arr = Arrow (ty, arr)

  let rec fn_eq : type a b. a fn -> b fn -> bool =
   fun fn1 fn2 ->
    match (fn1, fn2) with
    | (Returning t1, Returning t2) -> type_eq t1 t2
    | (Arrow (t1, f1), Arrow (t2, f2)) -> type_eq t1 t2 && fn_eq f1 f2
    | _ -> false
end
