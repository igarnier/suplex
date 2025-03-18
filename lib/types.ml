open Syntax

type 'a typ = 'a Syntax.typ

(** {2 Type-related functions} *)

let field_name : type a. (a, _) field -> string = function
  | Field { name; _ } -> name

let field_type : type a. (a, _) field -> a typ = function
  | Field { ty; _ } -> ty

(** Pretty-printing numerical types. *)
let pp_base_numerical : type a. Format.formatter -> a base_numerical -> unit =
 fun fmtr (n : a base_numerical) ->
  let open Format in
  match n with
  | I64_num -> fprintf fmtr "i64"
  | I32_num -> fprintf fmtr "i32"
  | I16_num -> fprintf fmtr "i16"
  | I8_num -> fprintf fmtr "i8"
  | I1_num -> fprintf fmtr "i1"
  | F32_num -> fprintf fmtr "f32"
  | F64_num -> fprintf fmtr "f64"

let pp_numerical : type a. Format.formatter -> a numerical -> unit =
 fun fmtr (n : a numerical) ->
  let open Format in
  match n with
  | Base_num n -> pp_base_numerical fmtr n
  | Vec_num { base; numel } ->
      fprintf fmtr "<%a x %d>" pp_base_numerical base (Size.to_int numel)

let rec pp_typ : type a. int list -> Format.formatter -> a typ -> unit =
 fun (type a) visited fmtr (typ : a typ) ->
  match typ with
  | TUnit -> Format.pp_print_string fmtr "unit"
  | TBool -> Format.pp_print_string fmtr "bool"
  | TNum n -> pp_numerical fmtr n
  | TPtr t -> Format.fprintf fmtr "Ptr(@[%a@])" (pp_typ visited) t
  | TArr_unk t -> Format.fprintf fmtr "<%a>" (pp_typ visited) t
  | TArr_cst (t, sz) -> Format.fprintf fmtr "<%a:%Ld>" (pp_typ visited) t sz
  | TRecord { descr } ->
      let rec loop : type x y z.
          int list -> Format.formatter -> (x, y, z, a) record_desc -> unit =
       fun visited fmtr descr ->
        match descr with
        | Record_empty -> ()
        | Record_field { field; index = _; rest } ->
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
              Format.fprintf fmtr "fix %d. %a" id (loop (id :: visited)) (f typ)
      in
      Format.fprintf fmtr "@[{" ;
      loop visited fmtr descr ;
      Format.fprintf fmtr "}@]"
  | TFn fn -> pp_fn visited fmtr fn

and pp_fn : type a. int list -> Format.formatter -> a fn -> unit =
 fun visited fmtr (fn : a fn) ->
  match fn with
  | Returning t -> pp_typ visited fmtr t
  | Arrow (t, f) ->
      Format.fprintf fmtr "%a -> %a" (pp_typ visited) t (pp_fn visited) f

let pp_typ fmtr typ = pp_typ [] fmtr typ

let pp_field : type a. Format.formatter -> (a, _) field -> unit =
 fun fmtr (Field { name; ty }) -> Format.fprintf fmtr "%s: %a" name pp_typ ty

let base_numerical_eq : type a b.
    a base_numerical -> b base_numerical -> (a, b) Type.eq option =
 fun n1 n2 ->
  match (n1, n2) with
  | (I64_num, I64_num) -> Some Equal
  | (I32_num, I32_num) -> Some Equal
  | (I16_num, I16_num) -> Some Equal
  | (I8_num, I8_num) -> Some Equal
  | (F64_num, F64_num) -> Some Equal
  | (F32_num, F32_num) -> Some Equal
  | _ -> None

let numerical_eq : type a b. a numerical -> b numerical -> (a, b) Type.eq option
    =
 fun n1 n2 ->
  match (n1, n2) with
  | (Base_num n1, Base_num n2) -> (
      match base_numerical_eq n1 n2 with
      | None -> None
      | Some Type.Equal -> Some Type.Equal)
  | (Vec_num { base = b1; numel = n1 }, Vec_num { base = b2; numel = n2 }) -> (
      match base_numerical_eq b1 b2 with
      | None -> None
      | Some Type.Equal -> (
          match Size.equal n1 n2 with
          | None -> None
          | Some Type.Equal -> Some Type.Equal))
  | _ -> None

let rec type_eq : type a b. a typ -> b typ -> bool =
 fun t1 t2 ->
  match (t1, t2) with
  | (TUnit, TUnit) -> true
  | (TBool, TBool) -> true
  | (TNum n1, TNum n2) -> (
      match numerical_eq n1 n2 with None -> false | Some Type.Equal -> true)
  | (TPtr t1, TPtr t2) -> type_eq t1 t2
  | (TArr_unk t1, TArr_unk t2) -> type_eq t1 t2
  | (TArr_cst (t1, sz1), TArr_cst (t2, sz2)) -> sz1 = sz2 && type_eq t1 t2
  | (TRecord { descr = descr1 }, TRecord { descr = descr2 }) ->
      record_eq descr1 descr2
  | _ -> false

and record_eq : type a b c u d e f v.
    (a, b, c, u) record_desc -> (d, e, f, v) record_desc -> bool =
 fun descr1 descr2 ->
  match (descr1, descr2) with
  | (Record_empty, Record_empty) -> true
  | ( Record_field { field = f1; index = i1; rest = r1 },
      Record_field { field = f2; index = i2; rest = r2 } ) ->
      field_eq f1 f2 && Int.equal i1 i2 && record_eq r1 r2
  | (Record_fix (id1, _), Record_fix (id2, _)) -> id1 = id2
  | _ -> false

and field_eq : type a b c d. (a, b) field -> (c, d) field -> bool =
 fun field1 field2 ->
  match (field1, field2) with
  | (Field field1, Field field2) ->
      String.equal field1.name field2.name && type_eq field1.ty field2.ty

let field_index : type a u.
    (a, u) field -> u typ -> (int option, string) Result.t =
 fun field typ ->
  let rec field_count : type x y z u. (x, y, z, u) record_desc -> int =
   fun desc ->
    match desc with
    | Record_empty -> 0
    | Record_field { rest; _ } -> 1 + field_count rest
    | Record_fix (_, _) -> assert false
  in
  let rec loop : type x y z a u.
      int ->
      (a, u) field ->
      (x, y, z, u) record_desc ->
      (int option, string) Result.t =
   fun field_count field descr ->
    match descr with
    | Record_empty -> Result.ok None
    | Record_field { field = f; index; rest } ->
        if field_eq f field then Result.ok (Some index)
        else loop field_count field rest
    | Record_fix (_, _) -> Result.error "found Record_fix inside of record"
  in
  match typ with
  | TRecord { descr = Record_fix (_, f) } ->
      let descr = f typ in
      let field_count = field_count descr in
      loop field_count field descr
  | TRecord { descr } ->
      let field_count = field_count descr in
      loop field_count field descr
  | _ -> Result.error "expected a record type"

let unit = TUnit

let bool = TBool

let base_num n = TNum (Base_num n)

let i64 = base_num I64_num

let i32 = base_num I32_num

let i16 = base_num I16_num

let i8 = base_num I8_num

let f32 = base_num F32_num

let f64 = base_num F64_num

let vec base numel = TNum (Vec_num { base; numel })

let ptr x = TPtr x

let arr typ = TArr_unk typ

let arr_cst ty sz =
  if sz < 0L then invalid_arg "Type_system.vec: negative static size" ;
  TArr_cst (ty, sz)

let empty_rec = Record_empty

let field : type a. string -> a typ -> (a, _) field =
 fun name ty -> Field { name; ty }

let next_index : type x y z u. (x, y, z, u) record_desc -> int =
 fun descr ->
  match descr with
  | Record_empty -> 0
  | Record_field { index; _ } -> index + 1
  | Record_fix (_, _) -> failwith "|+: found Record_fix inside of record"

let ( |+ ) rest field = Record_field { field; index = next_index rest; rest }

let gensym =
  let struct_id = ref 0 in
  fun () ->
    let id = !struct_id in
    incr struct_id ;
    id

let fix f =
  let id = gensym () in
  Record_fix (id, f)

let seal descr = TRecord { descr }

let projs : type elim x u. (elim, x Vec.t, x Vec.t, u) record_desc -> elim =
 fun record ->
  let rec loop : type elim acc. (elim, acc, x Vec.t, u) record_desc -> elim =
   fun descr ->
    match descr with
    | Record_empty -> (() : elim)
    | Record_field { field; index = _; rest } ->
        let elims = loop rest in
        ((elims, field) : elim)
    | Record_fix (_id, f) -> loop (f (seal descr))
  in
  loop record

let returning ty = Returning ty

let ( @-> ) ty arr = Arrow (ty, arr)

let rec fn_eq : type a b. a fn -> b fn -> bool =
 fun fn1 fn2 ->
  match (fn1, fn2) with
  | (Returning t1, Returning t2) -> type_eq t1 t2
  | (Arrow (t1, f1), Arrow (t2, f2)) -> type_eq t1 t2 && fn_eq f1 f2
  | _ -> false

(* Helpers *)

let assert_const_array ty ~expected_size =
  match ty with
  | TArr_cst (_, sz) when Int64.equal sz expected_size -> ()
  | _ ->
      Format.kasprintf
        failwith
        "store: type mismatch: value has type %a, expected array of size %Ld"
        pp_typ
        ty
        expected_size
