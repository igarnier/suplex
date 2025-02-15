open Syntax

(** {2 Relation between OCaml and LLVM values} *)

include (
  struct
    type 'a opaque = unit Ctypes.ptr

    let opaque = Ctypes.(ptr void)
  end :
    sig
      type 'a opaque

      val opaque : 'a opaque Ctypes.typ
    end)

type (_, _) num_rel =
  | I64_rel : (i64, int64) num_rel
  | I32_rel : (i32, int32) num_rel
  | I16_rel : (i16, int) num_rel
  | I8_rel : (i8, int) num_rel
  | F64_rel : (f64, float) num_rel
  | F32_rel : (f32, float) num_rel

module type BA = sig
  type elt

  type s

  val r :
    ( (unit * (i64, s) field) * ((elt, [ `unk ]) arr, s) field,
      ((elt, [ `unk ]) arr * (i64 * unit)) Vec.t,
      ((elt, [ `unk ]) arr * (i64 * unit)) Vec.t,
      s )
    record_desc

  val s : s typ

  val data : ((elt, [ `unk ]) arr, s) field

  val dim : (i64, s) field
end

module type BA_private = sig
  include BA

  type c_elt

  type c

  val s : s typ

  val ctypes_elt_ty : c_elt Ctypes.typ

  val ctypes : c Ctypes_static.structure Ctypes_static.typ

  val cdim : (int64, c Ctypes_static.structure) Ctypes_static.field

  val cdata :
    (c_elt Ctypes_static.ptr, c Ctypes_static.structure) Ctypes_static.field
end

type ('elt, 'c_elt, 'c) ba =
  (module BA_private
     with type elt = 'elt
      and type c_elt = 'c_elt
      and type c = 'c)

module Make_suplex_bigarray_struct (X : sig
  type s

  type c

  val rel : (s, c) num_rel
end) : BA_private with type elt = X.s and type c_elt = X.c = struct
  type elt = X.s

  type c_elt = X.c

  type s

  type c

  let numty : elt numerical =
    match X.rel with
    | I64_rel -> I64_num
    | I32_rel -> I32_num
    | I16_rel -> I16_num
    | I8_rel -> I8_num
    | F64_rel -> F64_num
    | F32_rel -> F32_num

  let elt_ty = TNum numty

  let r = Types.(empty_rec |+ field "dim" i64 |+ field "data" (arr elt_ty))

  let s : s typ = Types.seal r

  let (((), dim), data) = Types.projs r

  let ctypes_elt_ty : c_elt Ctypes.typ =
    match X.rel with
    | I64_rel -> Ctypes.int64_t
    | I32_rel -> Ctypes.int32_t
    | I16_rel -> Ctypes.int16_t
    | I8_rel -> Ctypes.int8_t
    | F32_rel -> Ctypes.float
    | F64_rel -> Ctypes.double

  let ((ctypes : c Ctypes_static.structure Ctypes_static.typ), cdim, cdata) =
    let open Ctypes in
    let strct = structure "i64_arr" in
    (* Careful: the order of fields matters here *)
    let data = field strct "data" (ptr ctypes_elt_ty) in
    let dim = field strct "dim" int64_t in
    seal strct ;
    (strct, dim, data)
end

module I64_ba = Make_suplex_bigarray_struct (struct
  type s = i64

  type c = int64

  let rel = I64_rel
end)

module I32_ba = Make_suplex_bigarray_struct (struct
  type s = i32

  type c = int32

  let rel = I32_rel
end)

module I16_ba = Make_suplex_bigarray_struct (struct
  type s = i16

  type c = int

  let rel = I16_rel
end)

module I8_ba = Make_suplex_bigarray_struct (struct
  type s = i8

  type c = int

  let rel = I8_rel
end)

module F64_ba = Make_suplex_bigarray_struct (struct
  type s = f64

  type c = float

  let rel = F64_rel
end)

module F32_ba = Make_suplex_bigarray_struct (struct
  type s = f32

  type c = float

  let rel = F32_rel
end)

type (_, _, _) full_rel =
  | Full_rel_unit : (unit expr, unit, bool) full_rel
  | Full_rel_bool : (bool expr, bool, bool) full_rel
  | Full_rel_int64 : (i64 expr, int64, int64) full_rel
  | Full_rel_int32 : (i32 expr, int32, int32) full_rel
  | Full_rel_int16 : (i16 expr, int, int) full_rel
  | Full_rel_int8 : (i8 expr, int, int) full_rel
  | Full_rel_float64 : (f64 expr, float, float) full_rel
  | Full_rel_float32 : (f32 expr, float, float) full_rel
  | Full_rel_string : (i8 ptr expr, string, string) full_rel
  | Full_rel_ba_i64 :
      ( I64_ba.s ptr expr,
        (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t,
        I64_ba.c Ctypes.structure Ctypes.ptr )
      full_rel
  | Full_rel_ba_i32 :
      ( I32_ba.s ptr expr,
        (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t,
        I32_ba.c Ctypes.structure Ctypes.ptr )
      full_rel
  | Full_rel_ba_i16 :
      ( I16_ba.s ptr expr,
        (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t,
        I16_ba.c Ctypes.structure Ctypes.ptr )
      full_rel
  | Full_rel_ba_i8 :
      ( I8_ba.s ptr expr,
        (int, Bigarray.int8_signed_elt, Bigarray.c_layout) Bigarray.Array1.t,
        I8_ba.c Ctypes.structure Ctypes.ptr )
      full_rel
  | Full_rel_ba_f64 :
      ( F64_ba.s ptr expr,
        (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t,
        F64_ba.c Ctypes.structure Ctypes.ptr )
      full_rel
  | Full_rel_ba_f32 :
      ( F32_ba.s ptr expr,
        (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t,
        F32_ba.c Ctypes.structure Ctypes.ptr )
      full_rel
  | Full_rel_arr_unk :
      ('suplex expr, 'ocaml, 'ctypes) full_rel
      -> ( ('suplex, [ `unk ]) arr expr,
           'ocaml Seq.t,
           'ctypes Ctypes.ptr )
         full_rel
  | Full_rel_arr_cst :
      int * ('suplex expr, 'ocaml, 'ctypes) full_rel
      -> ( ('suplex, [ `cst ]) arr expr,
           'ocaml Seq.t,
           'ctypes Ctypes.carray )
         full_rel
  | Full_rel_arr_cst_toplevel :
      int * ('suplex expr, 'ocaml, 'ctypes) full_rel
      -> ( ('suplex, [ `cst ]) arr expr,
           'ocaml Seq.t,
           'ctypes Ctypes.carray Ctypes.ptr )
         full_rel
      (** [Full_rel_arr_cst_toplevel] is not directly visible to the user. We
          silently replace [Full_rel_arr_cst] by this constructor to honor the
          fact that arrays are passed by-ref and not by-value *)
  | Full_rel_malloced_struct :
      (_, 'suplex Vec.t, 'suplex Vec.t, 'u) record_desc
      * ('suplex Vec.t, 'ocaml Vec.t, 'ctypes Vec.t) full_rel_vec
      -> ( 'u ptr expr,
           'ocaml Vec.t,
           'u Ctypes.structure Ctypes_static.ptr )
         full_rel
  | Full_rel_opaque_malloced_struct :
      (_, 'suplex Vec.t, 'suplex Vec.t, 'u) record_desc
      -> ('u ptr expr, 'u opaque, 'u opaque) full_rel
  | Full_rel_struct :
      (_, 'suplex Vec.t, 'suplex Vec.t, 'u) record_desc
      * ('suplex Vec.t, 'ocaml Vec.t, 'ctypes Vec.t) full_rel_vec
      -> ('u expr, 'ocaml Vec.t, 'u Ctypes.structure) full_rel

and (_, _, _) full_rel_vec =
  | [] : (unit Vec.t, unit Vec.t, unit Vec.t) full_rel_vec
  | ( :: ) :
      ('suplex expr, 'ocaml, 'ctypes) full_rel
      * ('stail Vec.t, 'otail Vec.t, 'ctail Vec.t) full_rel_vec
      -> ( ('suplex * 'stail) Vec.t,
           ('ocaml * 'otail) Vec.t,
           ('ctypes * 'ctail) Vec.t )
         full_rel_vec

type (_, _, _) full_rel_fn =
  | Fn_returning :
      ('suplex expr, 'ocaml, 'ctypes) full_rel
      -> ('suplex expr, 'ocaml, 'ctypes Ctypes.fn) full_rel_fn
  | Fn_arrow :
      ('sdom expr, 'odom, 'cdom) full_rel
      * ('srange, 'orange, 'crange Ctypes.fn) full_rel_fn
      -> ( 'sdom expr -> 'srange,
           'odom -> 'orange,
           ('cdom -> 'crange) Ctypes.fn )
         full_rel_fn

type ('suplex, 'ocaml) rel =
  | Rel : ('suplex, 'ocaml, 'ctypes) full_rel -> ('suplex, 'ocaml) rel
[@@ocaml.unboxed]

type ('suplex, 'ocaml) rel_vec =
  | Vec :
      ('suplex, 'ocaml, 'ctypes Vec.t) full_rel_vec
      -> ('suplex, 'ocaml) rel_vec
[@@ocaml.unboxed]

type ('suplex, 'ocaml) fn_rel =
  | Fn :
      ('suplex, 'ocaml, 'ctypes Ctypes.fn) full_rel_fn
      -> ('suplex, 'ocaml) fn_rel
[@@ocaml.unboxed]

(** {2 Top-level programs} *)

type _ wrapped_fundecl =
  | Fundecl :
      { fdecl : 's fundecl; rel : ('s, 'dom -> 'range) fn_rel }
      -> ('dom -> 'range) wrapped_fundecl

type _ module_ =
  | Empty_module : unit module_
  | Add_fundecl :
      { fdecl : ('dom -> 'range) wrapped_fundecl; mdl : 'r module_ }
      -> ('r * ('dom -> 'range)) module_

let empty_module = Empty_module

let add_fundecl fdecl mdl = Add_fundecl { fdecl; mdl }

type cfg = Llvm_executionengine.llcompileroptions

let rec extract_ctypes : type s o c. (s, o, c) full_rel -> c Ctypes.typ =
 fun r ->
  match r with
  | Full_rel_unit -> Ctypes.bool
  | Full_rel_bool -> Ctypes.bool
  | Full_rel_int64 -> Ctypes.int64_t
  | Full_rel_int32 -> Ctypes.int32_t
  | Full_rel_int16 -> Ctypes.int16_t
  | Full_rel_int8 -> Ctypes.int8_t
  | Full_rel_float64 -> Ctypes.double
  | Full_rel_float32 -> Ctypes.float
  | Full_rel_string -> Ctypes.string
  | Full_rel_ba_i64 -> Ctypes.ptr I64_ba.ctypes
  | Full_rel_ba_i32 -> Ctypes.ptr I32_ba.ctypes
  | Full_rel_ba_i16 -> Ctypes.ptr I16_ba.ctypes
  | Full_rel_ba_i8 -> Ctypes.ptr I8_ba.ctypes
  | Full_rel_ba_f64 -> Ctypes.ptr F64_ba.ctypes
  | Full_rel_ba_f32 -> Ctypes.ptr F32_ba.ctypes
  | Full_rel_arr_unk r -> Ctypes.ptr (extract_ctypes r)
  | Full_rel_arr_cst (len, strct) -> Ctypes.array len (extract_ctypes strct)
  | Full_rel_arr_cst_toplevel (len, strct) ->
      Ctypes.ptr (Ctypes.array len (extract_ctypes strct))
  | Full_rel_malloced_struct (_strct, v) -> Ctypes.ptr (ctypes_struct v)
  | Full_rel_opaque_malloced_struct _strct -> opaque
  | Full_rel_struct (_strct, v) -> ctypes_struct v

and ctypes_struct : type s o c u.
    (s Vec.t, o, c) full_rel_vec -> u Ctypes.structure Ctypes.typ =
 fun rels ->
  let open Ctypes in
  let p : u Ctypes.structure Ctypes_static.typ = structure "" in
  let rec loop : type s o c. (s Vec.t, o, c) full_rel_vec -> unit =
   fun rels ->
    match rels with
    | [] -> Ctypes.seal p
    | r :: tl ->
        let field_ty = extract_ctypes r in
        let _field = field p "" field_ty in
        loop tl
  in
  loop rels ;
  p

let bigarray_to_cstruct (type elt c_elt c) ((module BA) : (elt, c_elt, c) ba) v
    =
  let ptr = Ctypes.bigarray_start Ctypes.array1 v in
  let dim = Bigarray.Array1.dim v in
  let strct = Ctypes.make BA.ctypes in
  Ctypes.setf strct BA.cdim (Int64.of_int dim) ;
  Ctypes.setf strct BA.cdata ptr ;
  let res = Ctypes.allocate BA.ctypes strct in
  (* Keep the bigarray alive as long as [res] is alive. *)
  Gc.finalise (fun _ -> ignore v) res ;
  res

let rec ocaml_to_ctypes : type s o c. (s, o, c) full_rel -> o -> c =
 fun rel v ->
  match rel with
  | Full_rel_unit -> false
  | Full_rel_bool -> v
  | Full_rel_int64 -> v
  | Full_rel_int32 -> v
  | Full_rel_int16 -> v
  | Full_rel_int8 -> v
  | Full_rel_float64 -> v
  | Full_rel_float32 -> v
  | Full_rel_string -> v
  | Full_rel_ba_i64 -> bigarray_to_cstruct (module I64_ba) v
  | Full_rel_ba_i32 -> bigarray_to_cstruct (module I32_ba) v
  | Full_rel_ba_i16 -> bigarray_to_cstruct (module I16_ba) v
  | Full_rel_ba_i8 -> bigarray_to_cstruct (module I8_ba) v
  | Full_rel_ba_f64 -> bigarray_to_cstruct (module F64_ba) v
  | Full_rel_ba_f32 -> bigarray_to_cstruct (module F32_ba) v
  | Full_rel_arr_unk r ->
      let open Ctypes in
      let array = Array.of_seq v in
      let count = Array.length array in
      let ctypes_arr = allocate_n (extract_ctypes r) ~count in
      let cast = ocaml_to_ctypes r in
      let rec loop ptr n =
        if n = count then ()
        else
          let () = ptr <-@ cast array.(n) in
          loop (ptr +@ 1) (n + 1)
      in
      loop ctypes_arr 0 ;
      ctypes_arr
  | Full_rel_arr_cst (expected_len, r) ->
      let ctypes_arr = Ctypes.CArray.make (extract_ctypes r) expected_len in
      init_carray r v expected_len ctypes_arr ;
      ctypes_arr
  | Full_rel_arr_cst_toplevel (expected_len, r) ->
      let ptr_to_carray =
        Ctypes.allocate_n
          (Ctypes.array expected_len (extract_ctypes r))
          ~count:1
      in
      init_carray r v expected_len Ctypes.(!@ptr_to_carray) ;
      ptr_to_carray
  | Full_rel_malloced_struct (_strct, fields) ->
      let (typ, strct) = construct_ctypes_record fields v in
      Ctypes.allocate typ strct
  | Full_rel_opaque_malloced_struct _strct -> v
  | Full_rel_struct (_strct, fields) ->
      let (_typ, strct) = construct_ctypes_record fields v in
      strct

and init_carray : type s o c.
    (s, o, c) full_rel -> o Seq.t -> int -> c Ctypes_static.carray -> unit =
 fun r v expected_len ctypes_arr ->
  let open Ctypes in
  let cast = ocaml_to_ctypes r in
  let len = ref 0 in
  Seq.iteri
    (fun n x ->
      incr len ;
      if !len > expected_len then
        Format.kasprintf
          failwith
          "ocaml_to_ctypes: array initializer has length > %d"
          expected_len
      else () ;
      CArray.set ctypes_arr n (cast x))
    v

and construct_ctypes_record : type s o c u.
    (s Vec.t, o Vec.t, c) full_rel_vec ->
    o Vec.t ->
    u Ctypes.structure Ctypes_static.typ * u Ctypes.structure =
 fun rv v ->
  (* We reconstruct the Ctypes struct type and allocate it. *)
  let p : u Ctypes.structure Ctypes_static.typ = Ctypes.structure "" in
  let rec loop : type s o c.
      (s Vec.t, o Vec.t, c) full_rel_vec ->
      o Vec.t ->
      (u Ctypes.structure -> u Ctypes.structure) ->
      u Ctypes.structure =
   fun rv v k ->
    match (rv, v) with
    | ([], []) ->
        Ctypes.seal p ;
        let strct = Ctypes.make p in
        k strct
    | (r :: rtl, v :: vtl) ->
        let field_ty = extract_ctypes r in
        let field = Ctypes.field p "" field_ty in
        loop rtl vtl (fun strct ->
            Ctypes.setf strct field (ocaml_to_ctypes r v) ;
            k strct)
  in
  let strct = loop rv v Fun.id in
  (p, strct)

let rec ctypes_to_ocaml : type s o c. (s, o, c) full_rel -> c -> o =
 fun rel v ->
  match rel with
  | Full_rel_unit -> ()
  | Full_rel_bool -> v
  | Full_rel_int64 -> v
  | Full_rel_int32 -> v
  | Full_rel_int16 -> v
  | Full_rel_int8 -> v
  | Full_rel_float64 -> v
  | Full_rel_float32 -> v
  | Full_rel_string -> v
  | Full_rel_ba_i64 | Full_rel_ba_i32 | Full_rel_ba_i16 | Full_rel_ba_i8
  | Full_rel_ba_f64 | Full_rel_ba_f32 ->
      failwith "ctypes_to_ocaml: can't convert a bigarray to OCaml"
  | Full_rel_arr_unk _ ->
      failwith
        "ctypes_to_ocaml: can't convert an array with unknown length to OCaml"
  | Full_rel_arr_cst (_, _) ->
      failwith "ctypes_to_ocaml: can't convert an array to OCaml"
  | Full_rel_arr_cst_toplevel (_, _) ->
      failwith "ctypes_to_ocaml: can't convert an array to OCaml"
  | Full_rel_malloced_struct (_strct, fields) ->
      destruct_ctypes_record fields Ctypes.(!@v)
  | Full_rel_opaque_malloced_struct _ -> v
  | Full_rel_struct (_strct, fields) -> destruct_ctypes_record fields v

and destruct_ctypes_record : type s o c u.
    (s Vec.t, o Vec.t, c) full_rel_vec -> u Ctypes.structure -> o Vec.t =
 fun rv strct ->
  (* We lost the fields - hence we have reconstruct the Ctypes struct type in order
         to destruct [strct]. *)
  let p : u Ctypes.structure Ctypes_static.typ = Ctypes.structure "" in
  let rec loop : type s o' c.
      (s Vec.t, o' Vec.t, c) full_rel_vec -> (o' Vec.t -> o Vec.t) -> o Vec.t =
   fun rv k ->
    match rv with
    | [] -> k []
    | r :: rtl ->
        let field_ty = extract_ctypes r in
        let field = Ctypes.field p "" field_ty in
        loop rtl (fun rest ->
            let fv = Ctypes.getf strct field in
            k (ctypes_to_ocaml r fv :: rest))
  in
  loop rv Fun.id

let subst : type a b. (a, b) rel -> (a, b) rel =
 fun (Rel r as full) ->
  match r with
  | Full_rel_arr_cst (len, rel) -> Rel (Full_rel_arr_cst_toplevel (len, rel))
  | _ -> full

let rec box_ctypes_fn : type s o c. (s, o, c Ctypes.fn) full_rel_fn -> c -> o =
 fun rel f ->
  match rel with
  | Fn_returning r -> ctypes_to_ocaml r f
  | Fn_arrow (dom, range) ->
      fun x -> box_ctypes_fn range (f (ocaml_to_ctypes dom x))

let rec extract_ctypes_fn : type s o c. (s, o, c) full_rel_fn -> c =
 fun r ->
  match r with
  | Fn_returning ret -> Ctypes.returning (extract_ctypes ret)
  | Fn_arrow (dom, range) ->
      Ctypes.(extract_ctypes dom @-> extract_ctypes_fn range)

let rec extract_suplex : type s o c. (s expr, o, c) full_rel -> s typ =
 fun rel ->
  match rel with
  | Full_rel_unit -> Types.unit
  | Full_rel_bool -> Types.bool
  | Full_rel_int64 -> Types.i64
  | Full_rel_int32 -> Types.i32
  | Full_rel_int16 -> Types.i16
  | Full_rel_int8 -> Types.i8
  | Full_rel_float64 -> Types.f64
  | Full_rel_float32 -> Types.f32
  | Full_rel_string -> Types.ptr Types.i8
  | Full_rel_ba_i64 -> Types.ptr I64_ba.s
  | Full_rel_ba_i32 -> Types.ptr I32_ba.s
  | Full_rel_ba_i16 -> Types.ptr I16_ba.s
  | Full_rel_ba_i8 -> Types.ptr I8_ba.s
  | Full_rel_ba_f64 -> Types.ptr F64_ba.s
  | Full_rel_ba_f32 -> Types.ptr F32_ba.s
  | Full_rel_arr_unk r -> Types.arr (extract_suplex r)
  | Full_rel_arr_cst (len, r) ->
      Types.arr_cst (extract_suplex r) (Int64.of_int len)
  | Full_rel_arr_cst_toplevel (len, r) ->
      Types.arr_cst (extract_suplex r) (Int64.of_int len)
  | Full_rel_malloced_struct (r, _) -> Types.ptr (Types.seal r)
  | Full_rel_opaque_malloced_struct r -> Types.ptr (Types.seal r)
  | Full_rel_struct (r, _) -> Types.seal r

let rec prototype_of_rel : type s o c. (s, o, c) full_rel_fn -> s fn =
 fun rel ->
  match rel with
  | Fn_returning r -> Types.returning (extract_suplex r)
  | Fn_arrow (dom, range) ->
      Types.(extract_suplex dom @-> prototype_of_rel range)

let fundecl name (Fn rel) def =
  let sg = prototype_of_rel rel in
  fundecl name sg def

let run_module : type s. ?cfg:cfg -> ?state:Compile.llvm_state -> s module_ -> s
    =
 fun ?(cfg = Llvm_executionengine.default_compiler_options)
     ?(state = Compile.new_llvm_state ())
     mdl ->
  assert (Llvm_executionengine.initialize ()) ;
  let engine = Llvm_executionengine.create ~options:cfg state.llvm_module in
  (* let fpm = Llvm.PassManager.create () in *)
  (* ignore (Llvm.PassManager.run_module state.llvm_module fpm) ; *)
  let roots = ref List.[] in
  let rec loop : type s. s module_ -> environment -> s * environment =
   fun mdl env ->
    match mdl with
    | Empty_module -> ((), env)
    | Add_fundecl { fdecl = Fundecl { fdecl; rel = Fn rel }; mdl } -> (
        let (rest, env) = loop mdl env in
        let f = Compile.fundecl env state fdecl in
        let key = Hmap.Key.create () in
        let env = Hmap.add key f env in
        match rel with
        | Fn_returning _ ->
            (* Cannot be refuted because of abstract bigarray type *)
            assert false
        | Fn_arrow (_, _) ->
            let fn_ptr_typ = Foreign.funptr (extract_ctypes_fn rel) in
            let f =
              Llvm_executionengine.get_function_address
                fdecl.name
                fn_ptr_typ
                engine
            in
            let f = box_ctypes_fn rel f in
            roots := Obj.magic f :: !roots ;
            ((rest, f), env))
  in
  let (res, _) = loop mdl Hmap.empty in
  let roots_count = ref (List.length !roots) in
  List.iter
    (fun root ->
      Gc.finalise
        (fun _ ->
          decr roots_count ;
          if !roots_count = 0 then Llvm_executionengine.dispose engine)
        root)
    !roots ;
  res

let run_program (type s tdom trange) ?cfg ?state (fdecl : s fundecl)
    (rel : (s, tdom -> trange) fn_rel) : tdom -> trange =
  let ((), res) =
    run_module
      ?cfg
      ?state
      (Add_fundecl { fdecl = Fundecl { fdecl; rel }; mdl = Empty_module })
  in
  res

let run ?cfg ?(fname = "dummy") (Fn rel as fn_rel) body =
  let sg = prototype_of_rel rel in
  run_program ?cfg { name = fname; sg; body } fn_rel
