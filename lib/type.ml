module type S =
sig

  type 'a repr

  type unit_t   = Unit
  type int64_t  = Int64
  type int32_t  = Int32
  type int16_t  = Int16
  type int8_t   = Int8
  type char_t   = Char
  type bool_t   = Bool
  type float_t  = Float
  type double_t = Double
  type tuple_t  = Tuple
  type 'a ptr_t = Ptr
  type 'a vec_t = Vec

  type 'a numerical =
    | Int64 : int64_t numerical
    | Int32 : int32_t numerical
    | Int16 : int16_t numerical
    | Int8  : int8_t numerical
    | Float : float_t numerical
    | Double : double_t numerical

  type ('a, 'b) eq =
    | Refl_eq : ('a, 'a) eq

  type 'a typ =
    | TUnit : unit_t typ
    | TBool : bool_t typ
    | TNum  : 'a numerical -> 'a typ
    | TPtr  : 'a typ -> 'a ptr_t typ
    | TVec  : 'a typ -> 'a vec_t typ
    | TTpl  : ex_typ list -> tuple_t typ
    | TFun  : ('arg, 'ret) funtyp -> ('arg, 'ret) funtyp typ

  and ('a, 'b) funtyp =
    { arg : 'a typ; ret : 'b typ }

  and ex_typ =
    | Ex_typ : 'a typ -> ex_typ

  type ex_funtype =
    | Ex_funtype : ('a, 'b) funtyp -> ex_funtype

  type ex_repr =
    | Ex_repr : 'a repr -> ex_repr

  type 'a access = { index : int ; desired_type : 'a typ }


  val numerical_kind : 'a numerical -> [> `fp | `int ]
  val numerical_eq : 'a numerical -> 'b numerical -> ('a, 'b) eq option

  val type_eq : 'a typ -> 'b typ -> ('a, 'b) eq option
  val show_numerical : 'a numerical -> string
  val show_typ : 'a typ -> string

end

module type Repr =
sig
  type 'a repr
end

(* This functor allows us to decouple the specification of the language (which depends on types) 
   from the specification of the type system (which depnds on the type repr). *)
module Inst(R : Repr) =
struct

  type 'a repr = 'a R.repr

  (* base types *)
  type unit_t   = Unit   [@@deriving show]
  type int64_t  = Int64  [@@deriving show]
  type int32_t  = Int32  [@@deriving show]
  type int16_t  = Int16  [@@deriving show]
  type int8_t   = Int8   [@@deriving show]
  type char_t   = Char   [@@deriving show]
  type bool_t   = Bool   [@@deriving show]
  type float_t  = Float  [@@deriving show]
  type double_t = Double [@@deriving show]
  type tuple_t  = Tuple  [@@deriving show]
  type 'a ptr_t = Ptr    [@@deriving show]
  type 'a vec_t = Vec    [@@deriving show]

  (* classify "numerical" types *)
  type 'a numerical =
    | Int64 : int64_t numerical
    | Int32 : int32_t numerical
    | Int16 : int16_t numerical
    | Int8  : int8_t numerical
    | Float : float_t numerical
    | Double : double_t numerical

  let show_numerical : 'a . 'a numerical -> string =
    fun (type a) (n : a numerical) ->
      match n with
      | Int64 -> show_int64_t Int64
      | Int32 -> show_int32_t Int32
      | Int16 -> show_int16_t Int16
      | Int8 -> show_int8_t Int8
      | Float -> show_float_t Float
      | Double -> show_double_t Double

  let numerical_kind (type t) (n : t numerical) =
    match n with
    | Int64  -> `int
    | Int32  -> `int
    | Int16  -> `int
    | Int8   -> `int
    | Float  -> `fp
    | Double -> `fp

  type ('a, 'b) eq =
    | Refl_eq : ('a, 'a) eq

  type 'a typ =
    | TUnit : unit_t typ
    | TBool : bool_t typ
    | TNum  : 'a numerical -> 'a typ
    | TPtr  : 'a typ -> 'a ptr_t typ
    | TVec  : 'a typ -> 'a vec_t typ
    | TTpl  : ex_typ list -> tuple_t typ
    | TFun  : ('arg, 'ret) funtyp -> ('arg, 'ret) funtyp typ

  and ('a, 'b) funtyp =
    { arg : 'a typ ; ret : 'b typ }

  and ex_typ =
    | Ex_typ : 'a typ -> ex_typ


  type ex_funtype =
    | Ex_funtype : ('a, 'b) funtyp -> ex_funtype

  type ex_repr =
    | Ex_repr : 'a repr -> ex_repr

  type 'a access = { index : int ; desired_type : 'a typ }

  let numerical_eq : 'a 'b. 'a numerical -> 'b numerical -> ('a, 'b) eq option =
    fun (type a b) (x : a numerical) (y : b numerical) ->
      match x, y with
      | Int64, Int64 -> (Some Refl_eq : (a, b) eq option)
      | Int32, Int32 -> Some Refl_eq
      | Int16, Int16 -> Some Refl_eq
      | Int8, Int8 -> Some Refl_eq
      | Float, Float -> Some Refl_eq
      | Double, Double -> Some Refl_eq
      | _ -> None
  
  (* let atomic_eq : 'a atomic -> 'b atomic -> ('a, 'b) eq option =
   *   fun (type a b) (x : a atomic) (y : b atomic) ->
   *     match x, y with
   *     | Unit, Unit -> (Some Refl_eq : (a, b) eq option)
   *     | Bool, Bool -> Some Refl_eq
   *     | Num nx, Num ny -> numerical_eq nx ny
   *     | _ -> None *)

  let rec type_eq : type a b. a typ -> b typ -> (a, b) eq option =
    fun (type a b) (x : a typ) (y : b typ) ->
      match x, y with
      (* | TAtom ax, TAtom ay ->
       *   (match atomic_eq ax ay with
       *    | None -> None
       *    | Some Refl_eq ->
       *      Some Refl_eq : (a, b) eq option) *)
      | TNum nx, TNum ny -> numerical_eq nx ny
      | TUnit, TUnit -> Some Refl_eq
      | TBool, TBool -> Some Refl_eq
      | TPtr tx, TPtr ty ->
        (match type_eq tx ty with
         | None -> None
         | Some Refl_eq ->
           Some Refl_eq : (a, b) eq option)
      | TVec tx, TVec ty ->
        (match type_eq tx ty with
         | None -> None
         | Some Refl_eq ->
           Some Refl_eq : (a, b) eq option)
      | TTpl l1, TTpl l2 ->
        if ex_type_list_eq l1 l2 then
          Some Refl_eq
        else
          None
      | TFun { arg = arg1 ; ret = ret1 },
        TFun { arg = arg2 ; ret = ret2 } ->
        (match type_eq arg1 arg2 with
         | None -> None
         | Some Refl_eq ->
           match type_eq ret1 ret2 with
           | None -> None
           | Some Refl_eq ->
             Some Refl_eq)
      | _ ->
        None

  and ex_type_list_eq : ex_typ list -> ex_typ list -> bool =
    fun l1 l2 ->
      List.for_all2 (fun ex_typ1 ex_typ2 ->
          match ex_typ1, ex_typ2 with
          | Ex_typ typ1, Ex_typ typ2 ->
            (match type_eq typ1 typ2 with
             | None -> false
             | Some _ -> true)
        ) l1 l2

  let rec show_typ : 'a . 'a typ -> string =
    fun (type a) (typ : a typ) ->
      match typ with
      | TUnit -> "unit"
      | TBool -> "bool"
      | TNum n -> show_numerical n
      | TPtr t -> 
        let s = show_typ t in
        Printf.sprintf "[%s]" s
      | TVec t ->
        let s = show_typ t in
        Printf.sprintf "<%s>" s
      | TTpl typs ->
        let s = List.map (function Ex_typ t ->
            show_typ t
          ) typs in
        String.concat "," s
        |> Printf.sprintf "(%s)"
      | TFun { arg ; ret } ->
        let args = show_typ arg in
        let rets = show_typ ret in
        Printf.sprintf "%s -> %s" args rets
            
end
