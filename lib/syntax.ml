(* ------------------------------------------------------------------------------- *)

module Type = struct
  type unit_t = Unit

  type int64_t = Int64

  type int32_t = Int32

  type int16_t = Int16

  type int8_t = Int8

  type char_t = Char

  type bool_t = Bool

  type float32_t = Float32

  type float64_t = Float64

  type tuple_t = Tuple

  type 'a ptr_t = Ptr

  type 'a vec_t = Vec

  type !'a numerical =
    | Int64 : int64_t numerical
    | Int32 : int32_t numerical
    | Int16 : int16_t numerical
    | Int8 : int8_t numerical
    | Float32 : float32_t numerical
    | Float64 : float64_t numerical

  type !'a typ =
    | TUnit : unit_t typ
    | TBool : bool_t typ
    | TNum : 'a numerical -> 'a typ
    | TPtr : 'a typ -> 'a ptr_t typ
    | TVec : 'a typ -> 'a vec_t typ
    | TFun : ('arg, 'ret) funtyp -> ('arg, 'ret) funtyp typ
    | TRecord : ('i, 'e, 't) record -> ('i, 'e, 't) record typ

  and ('a, 'b) funtyp = { arg : 'a typ; ret : 'b typ }

  and ex_typ = Ex_typ : 'a typ -> ex_typ

  and ('intro, 'elim, 'full_intro, 't) record_descr =
    | Empty_record : ('intro, unit, 'intro, 't) record_descr
    | Add_elim :
        { field : ('a, 't) field; rest : ('a -> 'i, 'e, 'f, 't) record_descr }
        -> ('i, 'e * ('t -> 'a), 'f, 't) record_descr

  and ('a, 't) field = { name : string; ty : 'a typ }

  and ('intro, 'elim, 't) record =
    | Seal : ('t, 'elim, 'intro, 't) record_descr -> ('intro, 'elim, 't) record

  type ex_funtype = Ex_funtype : ('a, 'b) funtyp -> ex_funtype

  type 'a access = { index : int; desired_type : 'a typ }

  let pp_int64_t fmtr Int64 = Format.fprintf fmtr "int64"

  let pp_int32_t fmtr Int32 = Format.fprintf fmtr "int32"

  let pp_int16_t fmtr Int16 = Format.fprintf fmtr "int16"

  let pp_int8_t fmtr Int8 = Format.fprintf fmtr "int8"

  let pp_float64_t fmtr Float64 = Format.fprintf fmtr "float64"

  let pp_float32_t fmtr Float32 = Format.fprintf fmtr "float32"

  let pp_numerical : type a. Format.formatter -> a numerical -> unit =
    fun (type a) fmtr (n : a numerical) ->
     match n with
     | Int64 -> pp_int64_t fmtr Int64
     | Int32 -> pp_int32_t fmtr Int32
     | Int16 -> pp_int16_t fmtr Int16
     | Int8 -> pp_int8_t fmtr Int8
     | Float32 -> pp_float32_t fmtr Float32
     | Float64 -> pp_float64_t fmtr Float64

  let numerical_kind (type t) (n : t numerical) =
    match n with
    | Int64 -> `int
    | Int32 -> `int
    | Int16 -> `int
    | Int8 -> `int
    | Float32 -> `fp
    | Float64 -> `fp

  type ('a, 'b) eq = Refl_eq : ('a, 'a) eq

  let numerical_eq : type a b. a numerical -> b numerical -> (a, b) eq option =
    fun (type a b) (x : a numerical) (y : b numerical) ->
     match (x, y) with
     | (Int64, Int64) -> (Some Refl_eq : (a, b) eq option)
     | (Int32, Int32) -> Some Refl_eq
     | (Int16, Int16) -> Some Refl_eq
     | (Int8, Int8) -> Some Refl_eq
     | (Float32, Float32) -> Some Refl_eq
     | (Float64, Float64) -> Some Refl_eq
     | _ -> None

  let rec type_eq : type a b. a typ -> b typ -> (a, b) eq option =
    fun (type a b) (x : a typ) (y : b typ) ->
     match (x, y) with
     | (TNum nx, TNum ny) -> numerical_eq nx ny
     | (TUnit, TUnit) -> Some Refl_eq
     | (TBool, TBool) -> Some Refl_eq
     | (TPtr tx, TPtr ty) ->
         (match type_eq tx ty with None -> None | Some Refl_eq -> Some Refl_eq
           : (a, b) eq option)
     | (TVec tx, TVec ty) ->
         (match type_eq tx ty with None -> None | Some Refl_eq -> Some Refl_eq
           : (a, b) eq option)
     | (TFun { arg = arg1; ret = ret1 }, TFun { arg = arg2; ret = ret2 }) -> (
         match type_eq arg1 arg2 with
         | None -> None
         | Some Refl_eq -> (
             match type_eq ret1 ret2 with
             | None -> None
             | Some Refl_eq -> Some Refl_eq))
     | (TRecord _, TRecord _) ->
         (* TODO how to test equality of record descriptors *)
         assert false
     | _ -> None

  and ex_type_list_eq : ex_typ list -> ex_typ list -> bool =
   fun l1 l2 ->
    List.for_all2
      (fun ex_typ1 ex_typ2 ->
        match (ex_typ1, ex_typ2) with
        | (Ex_typ typ1, Ex_typ typ2) -> (
            match type_eq typ1 typ2 with None -> false | Some _ -> true))
      l1
      l2

  let rec pp_typ : type a. Format.formatter -> a typ -> unit =
    fun (type a) fmtr (typ : a typ) ->
     match typ with
     | TUnit -> Format.pp_print_string fmtr "unit"
     | TBool -> Format.pp_print_string fmtr "bool"
     | TNum n -> pp_numerical fmtr n
     | TPtr t -> Format.fprintf fmtr "[%a]" pp_typ t
     | TVec t -> Format.fprintf fmtr "<%a>" pp_typ t
     | TFun { arg; ret } -> Format.fprintf fmtr "%a -> %a" pp_typ arg pp_typ ret
     | TRecord (Seal descr) ->
         let rec loop :
             type w x y z. Format.formatter -> (w, x, y, z) record_descr -> unit
             =
          fun fmtr descr ->
           match descr with
           | Empty_record -> ()
           | Add_elim { field; rest } ->
               Format.fprintf fmtr "%s: %a" field.name pp_typ field.ty ;
               Format.fprintf fmtr ";@," ;
               loop fmtr rest
         in
         Format.fprintf fmtr "@[{" ;
         loop fmtr descr ;
         Format.fprintf fmtr "}@]"

  let unit = TUnit

  let bool = TBool

  let int64 = TNum Int64

  let int32 = TNum Int32

  let int16 = TNum Int16

  let int8 = TNum Int8

  let float32 = TNum Float32

  let float64 = TNum Float64

  let ptr x = TPtr x

  let vec x = TVec x

  let arrow arg ret = TFun { arg; ret }

  let empty_rec = Empty_record

  let field name ty = { name; ty }

  let ( |+ ) rest field = Add_elim { field; rest }

  let seal descr = TRecord (Seal descr)

  (* let make : ('intro, _, _) record typ -> 'intro =
   *  fun descr ->
   *   let rec loop : type x y intro z. (x, y, intro, z) record_descr -> intro =
   *    fun descr ->
   *     match descr with Empty_record f -> f | Add_elim { rest; _ } -> loop rest
   *   in
   *   let (TRecord (Seal descr)) = descr in
   *   loop descr
   *
   * let elims : (_, 'elim, _) record typ -> 'elim =
   *  fun descr ->
   *   let rec loop : type x elim y z. (x, elim, y, z) record_descr -> elim =
   *    fun descr ->
   *     match descr with
   *     | Empty_record _ -> ()
   *     | Add_elim { field; rest } ->
   *         let elims = loop rest in
   *         (elims, field.proj)
   *   in
   *   let (TRecord (Seal descr)) = descr in
   *   loop descr *)
end

module type S = sig
  open Type

  type !'a expr

  (** Stack frame specifications *)
  module Frame : sig
    type 'a stack_var =
      | Single : 'a ptr_t typ * 'a expr -> 'a ptr_t stack_var
      | Array : 'a ptr_t typ * 'a expr * int64_t expr -> 'a vec_t stack_var

    type (_, _) t =
      | [] : ('b, 'b) t
      | ( :: ) : 'a stack_var * ('c, 'b) t -> ('a expr -> 'c, 'b) t
  end

  (** Function signatures *)
  module Proto : sig
    type (_, _) t =
      | Return : 'a typ -> (unit, 'a expr) t
      | Cons : 'a typ * ('c, 'ret expr) t -> ('a expr * 'c, 'ret expr) t

    type _ args =
      | Done : unit args
      | Arg : 'a expr * 'c args -> ('a expr * 'c) args
  end

  type ('s, 'ret) fundecl

  type program

  type ex_expr = Ex_repr : 'a expr -> ex_expr

  val empty : program

  val unit : unit_t expr

  val int64 : int -> int64_t expr

  val int32 : int -> int32_t expr

  val int16 : int -> int16_t expr

  val int8 : int -> int8_t expr

  val float32 : float -> float32_t expr

  val float64 : float -> float64_t expr

  val tt : bool_t expr

  val ff : bool_t expr

  val ( && ) : bool_t expr -> bool_t expr -> bool_t expr

  val ( || ) : bool_t expr -> bool_t expr -> bool_t expr

  val add : 'a numerical -> 'a expr -> 'a expr -> 'a expr

  val sub : 'a numerical -> 'a expr -> 'a expr -> 'a expr

  val mul : 'a numerical -> 'a expr -> 'a expr -> 'a expr

  val div : 'a numerical -> 'a expr -> 'a expr -> 'a expr

  val lt : 'a numerical -> 'a expr -> 'a expr -> bool_t expr

  val le : 'a numerical -> 'a expr -> 'a expr -> bool_t expr

  val eq : 'a numerical -> 'a expr -> 'a expr -> bool_t expr

  val struct_ : ('intro, _, _) record typ -> 'intro

  val projs : (_, 'elim, _) record typ -> 'elim

  val seq : unit_t expr -> (unit -> 'a expr) -> 'a expr

  val ( let* ) : 'a expr -> ('a expr -> 'b expr) -> 'b expr

  val store : 'a ptr_t expr -> 'a expr -> unit_t expr

  val load : 'a ptr_t expr -> 'a expr

  val get : 'a vec_t expr -> int64_t expr -> 'a expr

  val set : 'a vec_t expr -> int64_t expr -> 'a expr -> unit_t expr

  val for_ :
    init:int64_t expr ->
    pred:(int64_t expr -> bool_t expr) ->
    step:(int64_t expr -> int64_t expr) ->
    (int64_t expr -> unit_t expr) ->
    unit_t expr

  val cond : bool_t expr -> (bool -> 'a expr) -> 'a expr

  val fundecl :
    name:string ->
    signature:('s, 'ret expr) Proto.t ->
    local:('b, 's -> 'ret expr) Frame.t ->
    body:'b ->
    (('s, 'ret expr) fundecl -> program) ->
    program

  val call : ('s, 'ret) fundecl -> 's Proto.args -> 'ret
end

module type T = sig
  include S

  type ('intro, 'elim, 't) descr =
    | Empty : (unit, unit, 't) descr
    | Field :
        'a Type.typ * ('t -> 'a) * ('i, 'e, 't) descr
        -> ('t -> 'i, 'e * ('t -> 'a), 't) descr

  val empty : (unit, unit, 't) descr

  val add_field :
    'a Type.typ ->
    ('t -> 'a) ->
    ('i, 'e, 't) descr ->
    ('t -> 'i, 'e * ('t -> 'a), 't) descr
end

module Native : sig
  include S

  val get_int64 : Type.int64_t expr -> int64
end = struct
  open Type

  type program = Program

  type (!'a, 'b) rel =
    | Unit_rel : (unit_t, unit) rel
    | Bool_rel : (bool_t, bool) rel
    | Int64_rel : (int64_t, int64) rel
    | Int32_rel : (int32_t, int32) rel
    | Int16_rel : (int16_t, int) rel
    | Int8_rel : (int8_t, int) rel
    | Float32_rel : (float32_t, float) rel
    | Float64_rel : (float64_t, float) rel
    | Ptr_rel : ('a, 'b) rel -> ('a ptr_t, 'b ref) rel
    | Vec_rel : ('a, 'b) rel -> ('a vec_t, 'b array) rel

  type !'a expr = Expr : { caml : 'b; rel : ('a, 'b) rel } -> 'a expr

  module Frame = struct
    type 'a stack_var =
      | Single : 'a ptr_t typ * 'a expr -> 'a ptr_t stack_var
      | Array : 'a ptr_t typ * 'a expr * int64_t expr -> 'a vec_t stack_var

    type (_, _) t =
      | [] : ('b, 'b) t
      | ( :: ) : 'a stack_var * ('c, 'b) t -> ('a expr -> 'c, 'b) t
  end

  module Proto = struct
    type (_, _) t =
      | Return : 'a typ -> (unit, 'a expr) t
      | Cons : 'a typ * ('c, 'ret expr) t -> ('a expr * 'c, 'ret expr) t

    type _ args =
      | Done : unit args
      | Arg : 'a expr * 'c args -> ('a expr * 'c) args
  end

  type ('a, 'ret) fundecl = 'a -> 'ret

  type ex_expr = Ex_repr : 'a expr -> ex_expr

  let rec elim_rel : type a b c. (a, b) rel -> (a, c) rel -> (b, c) eq =
   fun r1 r2 ->
    match (r1, r2) with
    | (Unit_rel, Unit_rel) -> Refl_eq
    | (Bool_rel, Bool_rel) -> Refl_eq
    | (Int64_rel, Int64_rel) -> Refl_eq
    | (Int32_rel, Int32_rel) -> Refl_eq
    | (Int16_rel, Int16_rel) -> Refl_eq
    | (Int8_rel, Int8_rel) -> Refl_eq
    | (Float32_rel, Float32_rel) -> Refl_eq
    | (Float64_rel, Float64_rel) -> Refl_eq
    | (Ptr_rel r, Ptr_rel r') -> (
        match elim_rel r r' with Refl_eq -> Refl_eq)
    | (Vec_rel r, Vec_rel r') -> (
        match elim_rel r r' with Refl_eq -> Refl_eq)

  let get_unit (Expr e : unit_t expr) : unit =
    match e.rel with Unit_rel -> e.caml

  let get_bool (Expr e : bool_t expr) : bool =
    match e.rel with Bool_rel -> e.caml

  let get_int64 (Expr e : int64_t expr) : int64 =
    match e.rel with Int64_rel -> e.caml

  let _get_int32 (Expr e : int32_t expr) : int32 =
    match e.rel with Int32_rel -> e.caml

  let _get_float32 (Expr e : float32_t expr) : float =
    match e.rel with Float32_rel -> e.caml

  let _get_float64 (Expr e : float64_t expr) : float =
    match e.rel with Float64_rel -> e.caml

  let empty = Program

  let unit = Expr { caml = (); rel = Unit_rel }

  let int64 i = Expr { caml = Int64.of_int i; rel = Int64_rel }

  let int32 i = Expr { caml = Int32.of_int i; rel = Int32_rel }

  let int16 i = Expr { caml = i; rel = Int16_rel }

  let int8 i = Expr { caml = i; rel = Int8_rel }

  let float32 f = Expr { caml = f; rel = Float32_rel }

  let float64 f = Expr { caml = f; rel = Float64_rel }

  let bool b = Expr { caml = b; rel = Bool_rel }

  let tt = bool true

  let ff = bool false

  let add : type a. a numerical -> a expr -> a expr -> a expr =
    fun (type a) (num : a numerical) (x : a expr) (y : a expr) ->
     let (Expr x) = x in
     let (Expr y) = y in
     match num with
     | Int64 ->
         (match (x.rel, y.rel) with
          | (Int64_rel, Int64_rel) ->
              int64 (Int64.to_int x.caml + Int64.to_int y.caml)
           : a expr)
     | Int32 -> (
         match (x.rel, y.rel) with
         | (Int32_rel, Int32_rel) ->
             int32 (Int32.to_int x.caml + Int32.to_int y.caml))
     | Int16 -> (
         match (x.rel, y.rel) with
         | (Int16_rel, Int16_rel) -> int16 (x.caml + y.caml))
     | Int8 -> (
         match (x.rel, y.rel) with
         | (Int8_rel, Int8_rel) -> int8 (x.caml + y.caml))
     | Float32 -> (
         match (x.rel, y.rel) with
         | (Float32_rel, Float32_rel) -> float32 (x.caml +. y.caml))
     | Float64 -> (
         match (x.rel, y.rel) with
         | (Float64_rel, Float64_rel) -> float64 (x.caml +. y.caml))

  let sub : type a. a numerical -> a expr -> a expr -> a expr =
    fun (type a) (num : a numerical) (x : a expr) (y : a expr) ->
     let (Expr x) = x in
     let (Expr y) = y in
     match num with
     | Int64 ->
         (match (x.rel, y.rel) with
          | (Int64_rel, Int64_rel) ->
              int64 (Int64.to_int x.caml - Int64.to_int y.caml)
           : a expr)
     | Int32 -> (
         match (x.rel, y.rel) with
         | (Int32_rel, Int32_rel) ->
             int32 (Int32.to_int x.caml - Int32.to_int y.caml))
     | Int16 -> (
         match (x.rel, y.rel) with
         | (Int16_rel, Int16_rel) -> int16 (x.caml - y.caml))
     | Int8 -> (
         match (x.rel, y.rel) with
         | (Int8_rel, Int8_rel) -> int8 (x.caml - y.caml))
     | Float32 -> (
         match (x.rel, y.rel) with
         | (Float32_rel, Float32_rel) -> float32 (x.caml -. y.caml))
     | Float64 -> (
         match (x.rel, y.rel) with
         | (Float64_rel, Float64_rel) -> float64 (x.caml -. y.caml))

  let mul : type a. a numerical -> a expr -> a expr -> a expr =
    fun (type a) (num : a numerical) (x : a expr) (y : a expr) ->
     let (Expr x) = x in
     let (Expr y) = y in
     match num with
     | Int64 ->
         (match (x.rel, y.rel) with
          | (Int64_rel, Int64_rel) ->
              int64 (Int64.to_int x.caml * Int64.to_int y.caml)
           : a expr)
     | Int32 -> (
         match (x.rel, y.rel) with
         | (Int32_rel, Int32_rel) ->
             int32 (Int32.to_int x.caml * Int32.to_int y.caml))
     | Int16 -> (
         match (x.rel, y.rel) with
         | (Int16_rel, Int16_rel) -> int16 (x.caml * y.caml))
     | Int8 -> (
         match (x.rel, y.rel) with
         | (Int8_rel, Int8_rel) -> int8 (x.caml * y.caml))
     | Float32 -> (
         match (x.rel, y.rel) with
         | (Float32_rel, Float32_rel) -> float32 (x.caml *. y.caml))
     | Float64 -> (
         match (x.rel, y.rel) with
         | (Float64_rel, Float64_rel) -> float64 (x.caml *. y.caml))

  let div : type a. a numerical -> a expr -> a expr -> a expr =
    fun (type a) (num : a numerical) (x : a expr) (y : a expr) ->
     let (Expr x) = x in
     let (Expr y) = y in
     match num with
     | Int64 ->
         (match (x.rel, y.rel) with
          | (Int64_rel, Int64_rel) ->
              int64 (Int64.to_int x.caml / Int64.to_int y.caml)
           : a expr)
     | Int32 -> (
         match (x.rel, y.rel) with
         | (Int32_rel, Int32_rel) ->
             int32 (Int32.to_int x.caml / Int32.to_int y.caml))
     | Int16 -> (
         match (x.rel, y.rel) with
         | (Int16_rel, Int16_rel) -> int16 (x.caml / y.caml))
     | Int8 -> (
         match (x.rel, y.rel) with
         | (Int8_rel, Int8_rel) -> int8 (x.caml / y.caml))
     | Float32 -> (
         match (x.rel, y.rel) with
         | (Float32_rel, Float32_rel) -> float32 (x.caml /. y.caml))
     | Float64 -> (
         match (x.rel, y.rel) with
         | (Float64_rel, Float64_rel) -> float64 (x.caml /. y.caml))

  let ( && ) : bool_t expr -> bool_t expr -> bool_t expr =
   fun x y ->
    let (Expr x) = x in
    let (Expr y) = y in
    match (x.rel, y.rel) with (Bool_rel, Bool_rel) -> bool (x.caml && y.caml)

  let ( || ) : bool_t expr -> bool_t expr -> bool_t expr =
   fun x y ->
    let (Expr x) = x in
    let (Expr y) = y in
    match (x.rel, y.rel) with (Bool_rel, Bool_rel) -> bool (x.caml || y.caml)

  let lt : type a. a numerical -> a expr -> a expr -> bool_t expr =
    fun (type a) (num : a numerical) (x : a expr) (y : a expr) ->
     let (Expr x) = x in
     let (Expr y) = y in
     match num with
     | Int64 ->
         (match (x.rel, y.rel) with
          | (Int64_rel, Int64_rel) -> bool (x.caml < y.caml)
           : bool_t expr)
     | Int32 -> (
         match (x.rel, y.rel) with
         | (Int32_rel, Int32_rel) -> bool (x.caml < y.caml))
     | Int16 -> (
         match (x.rel, y.rel) with
         | (Int16_rel, Int16_rel) -> bool (x.caml < y.caml))
     | Int8 -> (
         match (x.rel, y.rel) with
         | (Int8_rel, Int8_rel) -> bool (x.caml < y.caml))
     | Float32 -> (
         match (x.rel, y.rel) with
         | (Float32_rel, Float32_rel) -> bool (x.caml < y.caml))
     | Float64 -> (
         match (x.rel, y.rel) with
         | (Float64_rel, Float64_rel) -> bool (x.caml < y.caml))

  let le : type a. a numerical -> a expr -> a expr -> bool_t expr =
    fun (type a) (num : a numerical) (x : a expr) (y : a expr) ->
     let (Expr x) = x in
     let (Expr y) = y in
     match num with
     | Int64 ->
         (match (x.rel, y.rel) with
          | (Int64_rel, Int64_rel) -> bool (x.caml <= y.caml)
           : bool_t expr)
     | Int32 -> (
         match (x.rel, y.rel) with
         | (Int32_rel, Int32_rel) -> bool (x.caml <= y.caml))
     | Int16 -> (
         match (x.rel, y.rel) with
         | (Int16_rel, Int16_rel) -> bool (x.caml <= y.caml))
     | Int8 -> (
         match (x.rel, y.rel) with
         | (Int8_rel, Int8_rel) -> bool (x.caml <= y.caml))
     | Float32 -> (
         match (x.rel, y.rel) with
         | (Float32_rel, Float32_rel) -> bool (x.caml <= y.caml))
     | Float64 -> (
         match (x.rel, y.rel) with
         | (Float64_rel, Float64_rel) -> bool (x.caml <= y.caml))

  let eq : type a. a numerical -> a expr -> a expr -> bool_t expr =
    fun (type a) (num : a numerical) (x : a expr) (y : a expr) ->
     let (Expr x) = x in
     let (Expr y) = y in
     match num with
     | Int64 ->
         (match (x.rel, y.rel) with
          | (Int64_rel, Int64_rel) -> bool (x.caml = y.caml)
           : bool_t expr)
     | Int32 -> (
         match (x.rel, y.rel) with
         | (Int32_rel, Int32_rel) -> bool (x.caml = y.caml))
     | Int16 -> (
         match (x.rel, y.rel) with
         | (Int16_rel, Int16_rel) -> bool (x.caml = y.caml))
     | Int8 -> (
         match (x.rel, y.rel) with
         | (Int8_rel, Int8_rel) -> bool (x.caml = y.caml))
     | Float32 -> (
         match (x.rel, y.rel) with
         | (Float32_rel, Float32_rel) -> bool (x.caml = y.caml))
     | Float64 -> (
         match (x.rel, y.rel) with
         | (Float64_rel, Float64_rel) -> bool (x.caml = y.caml))

  let struct_ : ('intro, _, _) record typ -> 'intro = fun _ -> assert false

  let projs : (_, 'elim, _) record typ -> 'elim = fun _ -> assert false

  let seq (_m : unit_t expr) (f : unit -> 'b expr) = f ()

  let ( let* ) m f = f m

  let store : type a. a ptr_t expr -> a expr -> unit_t expr =
   fun (Expr lhs) (Expr rhs) ->
    match (lhs.rel, rhs.rel) with
    | (Ptr_rel lrel, rrel) ->
        (match elim_rel lrel rrel with Refl_eq -> lhs.caml := rhs.caml) ;
        unit

  let load : type a. a ptr_t expr -> a expr =
   fun (Expr ptr) ->
    match ptr.rel with Ptr_rel rel -> Expr { rel; caml = !(ptr.caml) }

  let get : type a. a vec_t expr -> int64_t expr -> a expr =
   fun (Expr vec) (Expr index) ->
    match (vec.rel, index.rel) with
    | (Vec_rel rel, Int64_rel) ->
        Expr { rel; caml = vec.caml.(Int64.to_int index.caml) }

  let set : type a. a vec_t expr -> int64_t expr -> a expr -> unit_t expr =
   fun (Expr vec) (Expr index) (Expr elt) ->
    match (vec.rel, elt.rel, index.rel) with
    | (Vec_rel vrel, erel, Int64_rel) ->
        (match elim_rel vrel erel with
        | Refl_eq -> vec.caml.(Int64.to_int index.caml) <- elt.caml) ;
        unit

  let for_ :
      init:int64_t expr ->
      pred:(int64_t expr -> bool_t expr) ->
      step:(int64_t expr -> int64_t expr) ->
      (int64_t expr -> unit_t expr) ->
      unit_t expr =
   fun ~init ~pred ~step body ->
    let i_ref = ref (get_int64 init) in
    let i () = int64 (Int64.to_int !i_ref) in
    let pred () = get_bool (pred (i ())) in
    while pred () do
      get_unit @@ body (i ()) ;
      i_ref := get_int64 (step (i ()))
    done ;
    unit

  let cond (Expr e : bool_t expr) (dispatch : bool -> 'a expr) =
    match e.rel with Bool_rel -> dispatch e.caml

  let rec alloca : type a b. (a, b) Frame.t -> a -> b =
   fun frame k ->
    match frame with
    | [] -> k
    | Single (_typ, Expr init) :: rest ->
        let expr = Expr { rel = Ptr_rel init.rel; caml = ref init.caml } in
        alloca rest (k expr)
    | Array (_typ, Expr init, Expr size) :: rest -> (
        match size.rel with
        | Int64_rel ->
            let expr =
              Expr
                { rel = Vec_rel init.rel;
                  caml = Array.make (Int64.to_int size.caml) init.caml
                }
            in
            alloca rest (k expr))

  let fundecl :
      name:string ->
      signature:('s, 'ret expr) Proto.t ->
      local:('b, 's -> 'ret expr) Frame.t ->
      body:'b ->
      (('s, 'ret expr) fundecl -> program) ->
      program =
   fun ~name:_ ~signature:_ ~local ~body scope -> scope (alloca local body)

  let rec call : type s ret. (s, ret) fundecl -> s Proto.args -> ret =
   fun f args ->
    match args with
    | Proto.Done -> f ()
    | Proto.Arg (expr, args) -> call (fun x -> f (expr, x)) args
end

let _fact_example =
  let open Native in
  fundecl
    ~name:"fact"
    ~signature:(Proto.Cons (Type.int64, Proto.Return Type.int64))
    ~local:[Single (Type.(ptr int64), int64 1)]
    ~body:(fun acc (n, ()) ->
      let* _ =
        for_
          ~init:(int64 1)
          ~pred:(fun i -> le Type.Int64 i n)
          ~step:(fun i -> add Type.Int64 i (int64 1))
          (fun i -> store acc (mul Type.Int64 (load acc) i))
      in
      load acc)
  @@ fun add ->
  let res = call add Proto.(Arg (int64 5, Done)) in
  let res = get_int64 res in
  let () = Format.printf "fact 5 = %Ld@." res in
  empty

module SMap = Map.Make (String)

module LLVM_state : sig
  type !'a llvm

  type !'a t

  val return : 'a -> 'a t

  val llreturn : Llvm.llvalue -> 'a Type.typ -> 'a llvm t

  val llval : 'a llvm -> Llvm.llvalue

  val typeof : 'a llvm -> 'a Type.typ

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val lmodule : Llvm.llmodule t

  val context : Llvm.llcontext t

  val builder : Llvm.llbuilder t

  val externals : Llvm.llvalue SMap.t t

  val set_externals : Llvm.llvalue SMap.t -> unit t

  val dump_module : unit t

  type llvm_state =
    { llvm_context : Llvm.llcontext;
      llvm_module : Llvm.llmodule;
      llvm_builder : Llvm.llbuilder;
      externals : Llvm.llvalue SMap.t
    }

  val run : 'a t -> llvm_state * 'a
end = struct
  type llvm_state =
    { llvm_context : Llvm.llcontext;
      llvm_module : Llvm.llmodule;
      llvm_builder : Llvm.llbuilder;
      externals : Llvm.llvalue SMap.t
    }

  type 'a llvm = { llval : Llvm.llvalue; typewit : 'a Type.typ }

  type 'a result = { state : llvm_state; result : 'a }

  type 'a t = llvm_state -> 'a result

  let ( let* ) (m : 'a t) (f : 'a -> 'b t) state =
    let mres = m state in
    f mres.result mres.state

  let return result state = { state; result }

  let llreturn llval typewit state = { state; result = { llval; typewit } }

  let llval { llval; _ } = llval

  let typeof { typewit; _ } = typewit

  let context state = { state; result = state.llvm_context }

  let lmodule state = { state; result = state.llvm_module }

  let builder state = { state; result = state.llvm_builder }

  let externals state = { state; result = state.externals }

  let set_externals externals state =
    let state = { state with externals } in
    { state; result = () }

  let fresh () =
    let llvm_context = Llvm.global_context () in
    let llvm_module = Llvm.create_module llvm_context "jit" in
    let llvm_builder = Llvm.builder llvm_context in
    let externals = SMap.empty in
    { llvm_context; llvm_module; llvm_builder; externals }

  let dump_module state =
    Llvm.dump_module state.llvm_module ;
    { state; result = () }

  let run x =
    assert (Llvm_executionengine.initialize ()) ;
    let { state; result } = x (fresh ()) in
    (state, result)
end

module LLVM_type = struct
  type t = Llvm.lltype

  let _void ctxt = Llvm.void_type ctxt

  let int64_t ctxt = Llvm.i64_type ctxt

  let int32_t ctxt = Llvm.i32_type ctxt

  let int16_t ctxt = Llvm.i16_type ctxt

  let int8_t ctxt = Llvm.i8_type ctxt

  let size_t ctxt = Llvm.i64_type ctxt

  let bool_t ctxt = Llvm.i1_type ctxt

  let float32_t ctxt = Llvm.float_type ctxt

  let float64_t ctxt = Llvm.double_type ctxt

  (* Convert type to Llvm repr *)
  let rec of_type : type a. a Type.typ -> t LLVM_state.t =
    fun (type a) (typ : a Type.typ) : t LLVM_state.t ->
     let open Type in
     let open LLVM_state in
     let* context in
     match typ with
     | TNum typ -> of_numerical typ
     | TUnit -> return (int8_t context)
     | TBool -> return (bool_t context)
     | TPtr typ ->
         let* lltyp = of_type typ in
         return (Llvm.pointer_type lltyp)
     | TVec typ ->
         let* lltyp = of_type typ in
         return (Llvm.pointer_type lltyp)
     | TFun { arg; ret } ->
         let* llrettyp = of_type ret in
         let* llargtyp = of_type arg in
         return (Llvm.function_type llrettyp [| llargtyp |])
     | TRecord (Seal record_descr) ->
         let* struct_type = struct_of_tuple record_descr in
         return struct_type

  and of_numerical : type a. a Type.numerical -> t LLVM_state.t =
    fun (type a) (typ : a Type.numerical) : Llvm.lltype LLVM_state.t ->
     let open LLVM_state in
     let open Type in
     let* context in
     match typ with
     | Int64 -> return (int64_t context)
     | Int32 -> return (int32_t context)
     | Int16 -> return (int16_t context)
     | Int8 -> return (int8_t context)
     | Float32 -> return (float32_t context)
     | Float64 -> return (float64_t context)

  and struct_of_tuple :
      type a b c d. (a, b, c, d) Type.record_descr -> t LLVM_state.t =
   fun descr ->
    let rec loop :
        type a b c d.
        (a, b, c, d) Type.record_descr ->
        Llvm.lltype list ->
        Llvm.lltype list LLVM_state.t =
     fun descr acc ->
      let open LLVM_state in
      match descr with
      | Type.Empty_record -> return (List.rev acc)
      | Type.Add_elim { field; rest } ->
          let* typ = of_type field.ty in
          loop rest (typ :: acc)
    in
    let open LLVM_state in
    let* context in
    let* res = loop descr [] in
    return (Llvm.struct_type context (Array.of_list res))
end

module LLVM_external = struct
  type name =
    | Print_int64
    | Print_int32
    | Print_int16
    | Print_int8
    | Print_unit
    | Print_bool
    | Print_float
    | Print_double
    | Instralloc

  type t = { name : name; typ : Type.ex_funtype }

  let to_string = function
    | Print_int64 -> "print_int64"
    | Print_int32 -> "print_int32"
    | Print_int16 -> "print_int16"
    | Print_int8 -> "print_int8"
    | Print_unit -> "print_unit"
    | Print_bool -> "print_bool"
    | Print_float -> "print_float"
    | Print_double -> "print_double"
    | Instralloc -> "instralloc"

  (* These are dummy external declaration so that the linker
     really does statically link those guys in the library. *)
  external print_int64 : unit -> unit = "print_int64"

  external print_int32 : unit -> unit = "print_int32"

  external print_int16 : unit -> unit = "print_int16"

  external print_int8 : unit -> unit = "print_int8"

  external print_unit : unit -> unit = "print_unit"

  external print_bool : unit -> unit = "print_bool"

  external print_float : unit -> unit = "print_float"

  external print_double : unit -> unit = "print_double"

  external instralloc : unit -> unit = "instralloc"

  let all =
    let open Type in
    [ { name = Print_int64; typ = Ex_funtype { arg = TNum Int64; ret = TUnit } };
      { name = Print_int32; typ = Ex_funtype { arg = TNum Int32; ret = TUnit } };
      { name = Print_int16; typ = Ex_funtype { arg = TNum Int16; ret = TUnit } };
      { name = Print_int8; typ = Ex_funtype { arg = TNum Int8; ret = TUnit } };
      { name = Print_float;
        typ = Ex_funtype { arg = TNum Float32; ret = TUnit }
      };
      { name = Print_double;
        typ = Ex_funtype { arg = TNum Float64; ret = TUnit }
      };
      { name = Print_bool; typ = Ex_funtype { arg = TBool; ret = TUnit } };
      { name = Print_unit; typ = Ex_funtype { arg = TUnit; ret = TUnit } };
      { name = Instralloc;
        typ = Ex_funtype { arg = TNum Int64; ret = TPtr (TNum Int8) }
      } ]

  let register_external (ext : t) : unit LLVM_state.t =
    let open LLVM_state in
    match ext.typ with
    | Type.Ex_funtype typ ->
        let* ft = LLVM_type.of_type (Type.TFun typ) in
        let* lmodule in
        let* externals in
        let name = to_string ext.name in
        let fv = Llvm.declare_function name ft lmodule in
        set_externals (SMap.add name fv externals)

  let register_all_externals : unit LLVM_state.t =
    let open LLVM_state in
    let rec loop l =
      match l with
      | [] -> return ()
      | ext :: tl ->
          let* () = register_external ext in
          loop tl
    in
    loop all

  let get_external ~name : Llvm.llvalue LLVM_state.t =
    let open LLVM_state in
    let* externals in
    let name = to_string name in
    match SMap.find_opt name externals with
    | None ->
        let msg = Printf.sprintf "Compile.get_external: %s not found" name in
        failwith msg
    | Some result -> return result

  module Malloc = struct
    let build_malloc_fptr lltype count =
      let open LLVM_state in
      let* builder in
      let* context in
      let* lmodule in
      match Llvm.lookup_function "instralloc" lmodule with
      | Some malloc ->
          let lltype = Llvm.pointer_type lltype in
          let sizeof = Llvm.size_of lltype in
          let bytesize = Llvm.build_mul count sizeof "multmp" builder in
          let downcast =
            Llvm.build_trunc
              bytesize
              (LLVM_type.size_t context)
              "casttmp"
              builder
          in
          let ptr = Llvm.build_call malloc [| downcast |] "calltmp" builder in
          let llvalue =
            Llvm.build_bitcast
              ptr
              (Llvm.pointer_type lltype)
              "alloc_ptr"
              builder
          in
          return llvalue
      | None -> failwith "build_malloc_fptr: instralloc not declared"

    let build_malloc_immediate lltype count =
      let open LLVM_state in
      let* builder in
      let* context in
      let* lmodule in
      match Llvm.lookup_function "instralloc" lmodule with
      | Some malloc ->
          let sizeof =
            Llvm.const_intcast
              (Llvm.size_of lltype)
              (LLVM_type.size_t context)
              ~is_signed:false
          in
          let bytesize = Llvm.build_mul count sizeof "multmp" builder in
          let downcast =
            Llvm.build_trunc
              bytesize
              (LLVM_type.size_t context)
              "casttmp"
              builder
          in
          let ptr = Llvm.build_call malloc [| downcast |] "calltmp" builder in
          let llvalue =
            Llvm.build_bitcast
              ptr
              (Llvm.pointer_type lltype)
              "alloc_ptr"
              builder
          in
          return llvalue
      | None -> failwith "build_malloc_immediate: instralloc not declared"

    let build_malloc (type a) (count : Llvm.llvalue) (typ : a Type.typ) :
        Llvm.llvalue LLVM_state.t =
      let open LLVM_state in
      let* lltype = LLVM_type.of_type typ in
      match typ with
      | Type.TFun _ -> build_malloc_fptr lltype count
      | Type.TUnit -> build_malloc_immediate lltype count
      | Type.TNum _ -> build_malloc_immediate lltype count
      | Type.(TRecord (Seal descr)) ->
          let* llstructtype = LLVM_type.struct_of_tuple descr in
          build_malloc_immediate llstructtype count
      | _ -> assert false
  end
end

module Repr = struct
  open Type

  type !'a expr = 'a LLVM_state.llvm LLVM_state.t

  module Frame = struct
    type 'a stack_var =
      | Single : 'a ptr_t typ * 'a expr -> 'a ptr_t stack_var
      | Array : 'a ptr_t typ * 'a expr * int64_t expr -> 'a vec_t stack_var

    type (_, _) t =
      | [] : ('b, 'b) t
      | ( :: ) : 'a stack_var * ('c, 'b) t -> ('a expr -> 'c, 'b) t
  end

  module Proto = struct
    type (_, _) t =
      | Return : 'a typ -> (unit, 'a expr) t
      | Cons : 'a typ * ('c, 'ret expr) t -> ('a expr * 'c, 'ret expr) t

    type _ args =
      | Done : unit args
      | Arg : 'a expr * 'c args -> ('a expr * 'c) args
  end

  type ('s, 'ret) fundecl =
    { name : string; signature : ('s, 'ret) Proto.t; fptr : Llvm.llvalue }

  type program = unit LLVM_state.t

  type ex_expr = Ex_repr : 'a expr -> ex_expr

  let empty = LLVM_state.return ()

  let unit : unit_t expr =
    let open LLVM_state in
    let* context in
    llreturn (Llvm.const_int (LLVM_type.int8_t context) 0) Type.unit

  let int64 (i : int) : int64_t expr =
    let open LLVM_state in
    let* context in
    llreturn (Llvm.const_int (LLVM_type.int64_t context) i) Type.int64

  let int32 (i : int) : int32_t expr =
    let open LLVM_state in
    let* context in
    llreturn (Llvm.const_int (LLVM_type.int32_t context) i) Type.int32

  let int16 (i : int) : int16_t expr =
    let open LLVM_state in
    let* context in
    llreturn (Llvm.const_int (LLVM_type.int16_t context) i) Type.int16

  let int8 (i : int) : int8_t expr =
    let open LLVM_state in
    let* context in
    llreturn (Llvm.const_int (LLVM_type.int8_t context) i) Type.int8

  let float32 (f : float) : float32_t expr =
    let open LLVM_state in
    let* context in
    llreturn (Llvm.const_float (LLVM_type.float32_t context) f) Type.float32

  let float64 (f : float) : float64_t expr =
    let open LLVM_state in
    let* context in
    llreturn (Llvm.const_float (LLVM_type.float64_t context) f) Type.float64

  let tt =
    let open LLVM_state in
    let* context in
    llreturn (Llvm.const_int (LLVM_type.bool_t context) 1) Type.bool

  let ff =
    let open LLVM_state in
    let* context in
    llreturn (Llvm.const_int (LLVM_type.bool_t context) 0) Type.bool

  let add (type t) (numtyp : t numerical) (lhs : t expr) (rhs : t expr) =
    let open LLVM_state in
    let* builder in
    let* lhs in
    let* rhs in
    let llval =
      match numerical_kind numtyp with
      | `int -> Llvm.build_add (llval lhs) (llval rhs) "int_add_tmp" builder
      | `fp -> Llvm.build_fadd (llval lhs) (llval rhs) "float_add_tmp" builder
    in
    llreturn llval (TNum numtyp)

  let sub (type t) (numtyp : t numerical) (lhs : t expr) (rhs : t expr) =
    let open LLVM_state in
    let* builder in
    let* lhs in
    let* rhs in
    let llval =
      match numerical_kind numtyp with
      | `int -> Llvm.build_sub (llval lhs) (llval rhs) "int_sub_tmp" builder
      | `fp -> Llvm.build_fsub (llval lhs) (llval rhs) "float_sub_tmp" builder
    in
    llreturn llval (TNum numtyp)

  let mul (type t) (numtyp : t numerical) (lhs : t expr) (rhs : t expr) =
    let open LLVM_state in
    let* builder in
    let* lhs in
    let* rhs in
    let llval =
      match numerical_kind numtyp with
      | `int -> Llvm.build_mul (llval lhs) (llval rhs) "int_mul_tmp" builder
      | `fp -> Llvm.build_fmul (llval lhs) (llval rhs) "float_mul_tmp" builder
    in
    llreturn llval (TNum numtyp)

  let div (type t) (numtyp : t numerical) (lhs : t expr) (rhs : t expr) =
    let open LLVM_state in
    let* builder in
    let* lhs in
    let* rhs in
    let llval =
      match numerical_kind numtyp with
      | `int -> Llvm.build_sdiv (llval lhs) (llval rhs) "int_sdiv_tmp" builder
      | `fp -> Llvm.build_fdiv (llval lhs) (llval rhs) "float_div_tmp" builder
    in
    llreturn llval (TNum numtyp)

  let ( && ) lhs rhs =
    let open LLVM_state in
    let* builder in
    let* lhs in
    let* rhs in
    let llval = Llvm.build_and (llval lhs) (llval rhs) "bool_and_tmp" builder in
    llreturn llval bool

  let ( || ) lhs rhs =
    let open LLVM_state in
    let* builder in
    let* lhs in
    let* rhs in
    let llval = Llvm.build_or (llval lhs) (llval rhs) "bool_or_tmp" builder in
    llreturn llval bool

  let lt (type t) (numtyp : t numerical) (lhs : t expr) (rhs : t expr) =
    let open LLVM_state in
    let* builder in
    let* lhs in
    let* rhs in
    let llval =
      match numerical_kind numtyp with
      | `int ->
          Llvm.build_icmp
            Llvm.Icmp.Slt
            (llval lhs)
            (llval rhs)
            "int_lt_tmp"
            builder
      | `fp ->
          Llvm.build_fcmp
            Llvm.Fcmp.Olt
            (llval lhs)
            (llval rhs)
            "float_lt_tmp"
            builder
    in
    llreturn llval bool

  let le (type t) (numtyp : t numerical) (lhs : t expr) (rhs : t expr) =
    let open LLVM_state in
    let* builder in
    let* lhs in
    let* rhs in
    let llval =
      match numerical_kind numtyp with
      | `int ->
          Llvm.build_icmp
            Llvm.Icmp.Sle
            (llval lhs)
            (llval rhs)
            "int_lt_tmp"
            builder
      | `fp ->
          Llvm.build_fcmp
            Llvm.Fcmp.Ole
            (llval lhs)
            (llval rhs)
            "float_le_tmp"
            builder
    in
    llreturn llval bool

  let eq (type t) (numtyp : t numerical) (lhs : t expr) (rhs : t expr) =
    let open LLVM_state in
    let* builder in
    let* lhs in
    let* rhs in
    let llval =
      match numerical_kind numtyp with
      | `int ->
          Llvm.build_icmp
            Llvm.Icmp.Eq
            (llval lhs)
            (llval rhs)
            "int_eq_tmp"
            builder
      | `fp ->
          Llvm.build_fcmp
            Llvm.Fcmp.Oeq
            (llval lhs)
            (llval rhs)
            "float_eq_tmp"
            builder
    in
    llreturn llval bool

  let struct_ : ('intro, _, _) record typ -> 'intro = fun _ -> assert false

  let projs : (_, 'elim, _) record typ -> 'elim = fun _ -> assert false

  let seq (m : unit_t expr) (f : unit -> 'b expr) =
    let open LLVM_state in
    let* _m = m in
    f ()

  let ( let* ) (m : 'a expr) (f : 'a expr -> 'b expr) : 'b expr =
    let open LLVM_state in
    let* m in
    f (return m)

  let store (type a) (ptr : a ptr_t expr) (v : a expr) : unit_t expr =
    let open LLVM_state in
    let* builder in
    let* ptr in
    let* v in
    let _ = Llvm.build_store (llval v) (llval ptr) builder in
    unit

  let load (type a) (ptr : a ptr_t expr) : a expr =
    let open LLVM_state in
    let* builder in
    let* ptr in
    match typeof ptr with
    | TPtr typ -> llreturn (Llvm.build_load (llval ptr) "load_tmp" builder) typ

  let get (type a) (arr : a vec_t expr) (i : int64_t expr) : a expr =
    let open LLVM_state in
    let* builder in
    let* arr in
    let* i in
    match (typeof arr, typeof i) with
    | (TVec typ, TNum Int64) ->
        let addr = Llvm.build_gep (llval arr) [| llval i |] "get_gep" builder in
        llreturn (Llvm.build_load addr "get_load_tmp" builder) typ

  let set (type a) (arr : a vec_t expr) (i : int64_t expr) (e : a expr) :
      unit_t expr =
    let open LLVM_state in
    let* builder in
    let* arr in
    let* i in
    let* e in
    let addr = Llvm.build_gep (llval arr) [| llval i |] "get_gep" builder in
    let _ = Llvm.build_store addr (llval e) builder in
    unit

  let cond (type t) (cond : bool_t expr) (dispatch : bool -> t expr) =
    let open LLVM_state in
    let* builder in
    let* context in
    let* cond in
    let end_of_cond_pos = Llvm.insertion_block builder in
    let enclosing_func = Llvm.block_parent end_of_cond_pos in
    (* append a basic block in the current function *)
    let trueblock = Llvm.append_block context "iftrue" enclosing_func in
    (* position the instruction writer at the end of that new block
           (which is also the beginning since [iftrue] is empty) *)
    Llvm.position_at_end trueblock builder ;
    (* codegen into that block *)
    let* bt = dispatch true in
    (* since [codegen_expr] can create new block, we need to get the
           actual block we are in when finishing [codegen_expr gamma iftrue].
           This [trueblock'] is the actual predecessor of the 'continuation'
           block (i.e. the [phi] block). *)
    let trueblock' = Llvm.insertion_block builder in
    let falseblock = Llvm.append_block context "iffalse" enclosing_func in
    Llvm.position_at_end falseblock builder ;
    let* bf = dispatch false in
    let falseblock' = Llvm.insertion_block builder in
    (* emit code for the 'join' aka 'continuation' aka 'phi' block *)
    let joinblock = Llvm.append_block context "ifjoin" enclosing_func in
    Llvm.position_at_end joinblock builder ;
    let incoming = List.[(llval bt, trueblock'); (llval bf, falseblock')] in
    let phi = Llvm.build_phi incoming "phitmp" builder in
    (* move the instruction builder back at he the end of [end_of_cond_pos],
           emit appropriate jump. *)
    Llvm.position_at_end end_of_cond_pos builder ;
    ignore (Llvm.build_cond_br (llval cond) trueblock falseblock builder) ;
    (* insert jumps from end of the 'true branch' block to the merge node. *)
    Llvm.position_at_end trueblock' builder ;
    ignore (Llvm.build_br joinblock builder) ;
    (* insert jumps from end of the 'true branch' block to the merge node. *)
    Llvm.position_at_end falseblock' builder ;
    ignore (Llvm.build_br joinblock builder) ;
    (* Move inserter at end of join block. *)
    Llvm.position_at_end joinblock builder ;
    llreturn phi (typeof bf)

  (* TODO initialization *)
  let rec alloca :
      type a s ret.
      (a, s -> ret expr) Frame.t -> a -> (s -> ret expr) LLVM_state.t =
    let open LLVM_state in
    fun (type a s ret) (frame : (a, s -> ret expr) Frame.t) (k : a) ->
      match frame with
      | [] -> LLVM_state.return (k : s -> ret expr)
      | Single (ptr_typ, _init) :: rest -> (
          match ptr_typ with
          | TPtr typ ->
              let* builder in
              let* lltyp = LLVM_type.of_type typ in
              let llalloca = Llvm.build_alloca lltyp "loc" builder in
              let expr = llreturn llalloca ptr_typ in
              alloca rest (k expr))
      | Array (ptr_typ, _init, size) :: rest -> (
          let* size in
          match ptr_typ with
          | TPtr typ ->
              let* builder in
              let* lltyp = LLVM_type.of_type typ in
              let llalloca =
                Llvm.build_array_alloca
                  lltyp
                  (llval size)
                  "alloca_array_tmp"
                  builder
              in
              let expr = llreturn llalloca (vec typ) in
              alloca rest (k expr))

  let rec prototype :
      type s ret.
      (s, ret expr) Proto.t -> Llvm.lltype list -> Llvm.lltype LLVM_state.t =
    fun (type s ret) (proto : (s, ret expr) Proto.t) acc ->
     let open LLVM_state in
     match proto with
     | Proto.Return ty ->
         let* retty = LLVM_type.of_type ty in
         return (Llvm.function_type retty (Array.of_list (List.rev acc)))
     | Proto.Cons (ty, rest) ->
         let* llty = LLVM_type.of_type ty in
         prototype rest (llty :: acc)

  let rec args_to_tuple :
      type s ret. (s, ret expr) Proto.t -> Llvm.llvalue list -> s option =
   fun proto args ->
    let open LLVM_state in
    match (proto, args) with
    | (Proto.Return _, []) -> Some ()
    | (Proto.Cons (typ, rest), arg :: args) ->
        Option.map
          (fun rest -> (llreturn arg typ, rest))
          (args_to_tuple rest args)
    | _ -> None

  let fundecl :
      name:string ->
      signature:('s, 'ret expr) Proto.t ->
      local:('b, 's -> 'ret expr) Frame.t ->
      body:'b ->
      (('s, 'ret expr) fundecl -> program) ->
      program =
   fun ~name ~signature ~local ~body scope ->
    let open LLVM_state in
    let* lmodule in
    let* builder in
    let* context in
    let signature = signature in
    let* proto = prototype signature [] in
    let fn = Llvm.declare_function name proto lmodule in
    let params = Llvm.params fn in
    match args_to_tuple signature (Array.to_list params) with
    | None ->
        failwith "fundecl: LLVM function parameters do not match declared arity"
    | Some args ->
        let bb = Llvm.append_block context "entry" fn in
        Llvm.position_at_end bb builder ;
        let* function_closed_on_local_variables = alloca local body in
        let* res = function_closed_on_local_variables args in
        let _ = Llvm.build_ret (llval res) builder in
        let fundecl = { name; signature; fptr = fn } in
        Llvm_analysis.assert_valid_function fn ;
        scope fundecl

  let call : type s ret. (s, ret) fundecl -> s Proto.args -> ret =
   fun f args ->
    let open LLVM_state in
    let rec loop :
        type s ret.
        (s, ret) Proto.t -> s Proto.args -> Llvm.llvalue list t -> ret =
     fun proto args acc ->
      match (proto, args) with
      | (Proto.Return retty, Proto.Done) ->
          let* builder in
          let* acc in
          let call =
            Llvm.build_call f.fptr (Array.of_list (List.rev acc)) "call" builder
          in
          llreturn call retty
      | (Proto.Cons (_ty, proto), Proto.Arg (expr, args)) ->
          loop
            proto
            args
            (let* expr in
             let* acc in
             return (llval expr :: acc))
    in
    loop f.signature args (return [])

  let for_ :
      init:int64_t expr ->
      pred:(int64_t expr -> bool_t expr) ->
      step:(int64_t expr -> int64_t expr) ->
      (int64_t expr -> unit_t expr) ->
      unit_t expr =
   fun ~init ~pred ~step body ->
    let open LLVM_state in
    let* builder in
    let* context in

    let current_block = Llvm.insertion_block builder in
    let enclosing_func = Llvm.block_parent current_block in

    let for_init = Llvm.append_block context "for_init" enclosing_func in
    let for_entry = Llvm.append_block context "for_entry" enclosing_func in
    let for_body = Llvm.append_block context "for_body" enclosing_func in
    let for_exit = Llvm.append_block context "for_exit" enclosing_func in

    (* Add unconditional jump from [current_block] inherited from context to [for_init] *)
    let _ = Llvm.build_br for_init builder in
    (* codegen init *)
    Llvm.position_at_end for_init builder ;
    let* init in
    let _ = Llvm.build_br for_entry builder in

    let last_init_block = Llvm.insertion_block builder in

    Llvm.position_at_end for_entry builder ;

    let* phi_ty = LLVM_type.of_type Type.int64 in
    let phi = Llvm.build_empty_phi phi_ty "for_phi" builder in
    Llvm.add_incoming (llval init, last_init_block) phi ;
    let phi_expr = llreturn phi Type.int64 in
    let* cond = pred phi_expr in
    let _ = Llvm.build_cond_br (llval cond) for_body for_exit builder in

    Llvm.position_at_end for_body builder ;
    seq (body phi_expr) @@ fun () ->
    let* next = step phi_expr in
    let _ = Llvm.build_br for_entry builder in

    Llvm.add_incoming (llval next, Llvm.insertion_block builder) phi ;
    Llvm.position_at_end for_exit builder ;
    unit
end

let fact_example =
  let open Repr in
  fundecl
    ~name:"fact"
    ~signature:(Proto.Cons (Type.int64, Proto.Return Type.int64))
    ~local:
      [ Single (Type.(ptr int64), int64 1);
        Single
          ( Type.ptr
              Type.(
                empty_rec |+ field "f1" float32 |+ field "f2" float32 |> seal),
            Obj.magic 0 ) ]
    ~body:(fun acc _strct (n, ()) ->
      seq (store acc (int64 1)) @@ fun () ->
      let* _ =
        for_
          ~init:(int64 1)
          ~pred:(fun i -> le Type.Int64 i n)
          ~step:(fun i -> add Type.Int64 i (int64 1))
          (fun i -> store acc (mul Type.Int64 (load acc) i))
      in
      load acc)
  @@ fun fact ->
  fundecl
    ~name:"main"
    ~signature:Proto.(Return Type.int64)
    ~local:[]
    ~body:(fun () -> call fact Proto.(Arg (int64 5, Done)))
  @@ fun _main -> empty

(* let (state, ()) = LLVM_state.run fact_example
 *
 * let res =
 *   let engine = Llvm_executionengine.create state.llvm_module in
 *
 *   let fpm = Llvm.PassManager.create () in
 *   Llvm.dump_module state.llvm_module ;
 *   let _ = Llvm.PassManager.run_module state.llvm_module fpm in
 *
 *   let fn_typ : (unit -> int64) Ctypes.fn =
 *     Ctypes.(void @-> returning int64_t)
 *   in
 *   let fn_ptr_typ = Foreign.funptr fn_typ in
 *   let f = Llvm_executionengine.get_function_address "main" fn_ptr_typ engine in
 *   let res = f () in
 *   let () = Format.printf "executing = %Ld@." res in
 *   Llvm_executionengine.dispose engine *)
