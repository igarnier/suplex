open Intf
open Function
module SMap = Map.Make (String)

type llvm_state =
  { llvm_context : Llvm.llcontext;
    llvm_module : Llvm.llmodule;
    llvm_builder : Llvm.llbuilder;
    externals : Llvm.llvalue SMap.t;
    const_strings : (string, Llvm.llvalue) Hashtbl.t
  }

exception Invalid_llvm_function of Llvm.llmodule * Llvm.llvalue

type i64 = Type_system.i64 = I64

type i32 = Type_system.i32 = I32

type i16 = Type_system.i16 = I16

type i8 = Type_system.i8 = I8

type f32 = Type_system.f32 = F32

type f64 = Type_system.f64 = F64

type 'a numerical = 'a Type_system.numerical =
  | I64_num : Type_system.i64 numerical
  | I32_num : Type_system.i32 numerical
  | I16_num : Type_system.i16 numerical
  | I8_num : Type_system.i8 numerical
  | F32_num : Type_system.f32 numerical
  | F64_num : Type_system.f64 numerical

module Impl : sig
  include
    S
      with type K.state = llvm_state
       and type Exec.cfg = Llvm_executionengine.llcompileroptions

  val set_debug_metadata : string -> unit
end = struct
  type ('a, 'typ) typed_llvm = { llval : Llvm.llvalue; typewit : 'typ }

  type ('a, 'typ) expr = ('a, 'typ) typed_llvm option

  type 'a ptr = Ptr [@@ocaml.warning "-37"]

  type (!'a, 'c) arr = Arr [@@ocaml.warning "-37"]

  module K : sig
    type ('a, 'typ) llvm = ('a, 'typ) expr

    type 'a result = { state : llvm_state; result : 'a }

    type 'a t = llvm_state -> 'a result

    include Intf.Monad with type 'a t := 'a t and type state = llvm_state

    val ( let*? ) :
      ('a, 'typ) llvm t ->
      (('a, 'typ) typed_llvm -> 'b option t) ->
      llvm_state ->
      'b option result

    val llreturn : Llvm.llvalue -> 'typ -> ('a, 'typ) llvm t

    val llreturn_unsound : ('a, 'typ) llvm t

    val llval : (_, _) typed_llvm -> Llvm.llvalue

    val typeof : (_, 'typ) typed_llvm -> 'typ

    val lmodule : Llvm.llmodule t

    val context : Llvm.llcontext t

    val builder : Llvm.llbuilder t

    val externals : Llvm.llvalue SMap.t t [@@ocaml.warning "-32"]

    val global_string : z:bool -> string -> Llvm.llvalue t

    val set_externals : Llvm.llvalue SMap.t -> unit t [@@ocaml.warning "-32"]

    val dump_module : unit t [@@ocaml.warning "-32"]
  end = struct
    type ('a, 'typ) llvm = ('a, 'typ) expr

    type 'a result = { state : llvm_state; result : 'a }

    type 'a t = llvm_state -> 'a result

    let ( let* ) (m : 'a t) (f : 'a -> 'b t) state =
      let mres = m state in
      f mres.result mres.state

    let ( let*? ) (type a b) (m : (a, 'typ) llvm t)
        (f : (a, 'typ) typed_llvm -> b option t) state =
      let mres = m state in
      match mres.result with
      | None -> { mres with result = None }
      | Some res -> f res mres.state

    let return result state = { state; result }

    let llreturn llval typewit state =
      { state; result = Some { llval; typewit } }

    let llreturn_unsound state = { state; result = None }

    let llval { llval; _ } = llval

    let typeof { typewit; _ } = typewit

    let context state = { state; result = state.llvm_context }

    let lmodule state = { state; result = state.llvm_module }

    let builder state = { state; result = state.llvm_builder }

    let externals state = { state; result = state.externals }

    let set_externals externals state =
      let state = { state with externals } in
      { state; result = () }

    let global_string ~z s (state : llvm_state) =
      match Hashtbl.find_opt state.const_strings s with
      | Some llglob ->
          (* llglob is a fixed-size array! *)
          { state; result = llglob }
      | None ->
          let llglob =
            Llvm.define_global
              s
              ((if z then Llvm.const_stringz else Llvm.const_string)
                 state.llvm_context
                 s)
              state.llvm_module
          in
          Hashtbl.add state.const_strings s llglob ;
          { state; result = llglob }

    let fresh () =
      let llvm_context = Llvm.global_context () in
      let llvm_module = Llvm.create_module llvm_context "jit" in
      let llvm_builder = Llvm.builder llvm_context in
      let externals = SMap.empty in
      let const_strings = Hashtbl.create 11 in
      (* builtins *)
      let typ =
        Llvm.function_type
          (Llvm.i1_type llvm_context)
          [| Llvm.pointer_type llvm_context |]
      in
      ignore (Llvm.declare_function "failwith" typ llvm_module) ;
      ignore (Llvm.declare_function "print" typ llvm_module) ;
      { llvm_context; llvm_module; llvm_builder; externals; const_strings }

    let dump_module state =
      Llvm.dump_module state.llvm_module ;
      { state; result = () }

    type cfg = unit

    let default_cfg () = ()

    type state = llvm_state

    let run () x =
      assert (Llvm_executionengine.initialize ()) ;
      let { state; result } = x (fresh ()) in
      (state, result)
  end

  type 'a k = 'a K.t

  module Types :
    Type_system.S
      with type ('a, 'b) typed_term = ('a, 'b) expr k
       and type 'a ptr = 'a ptr
       and type ('a, 'c) arr = ('a, 'c) arr = Type_system.Make (struct
    type ('a, 'b) typed_term = ('a, 'b) expr k

    type nonrec 'a ptr = 'a ptr

    type nonrec ('a, 'c) arr = ('a, 'c) arr
  end)

  type 'a m = 'a Types.m

  type 'a typ = 'a Types.typ

  open Types

  module LLVM_type = struct
    type t = Llvm.lltype

    let _void_t ctxt = Llvm.void_type ctxt

    let int64_t ctxt = Llvm.i64_type ctxt

    let int32_t ctxt = Llvm.i32_type ctxt

    let int16_t ctxt = Llvm.i16_type ctxt [@@ocaml.warning "-32"]

    let int8_t ctxt = Llvm.i8_type ctxt [@@ocaml.warning "-32"]

    let size_t ctxt = Llvm.i64_type ctxt [@@ocaml.warning "-32"]

    let bool_t ctxt = Llvm.i1_type ctxt

    let float32_t ctxt = Llvm.float_type ctxt

    let float64_t ctxt = Llvm.double_type ctxt

    let struct_table = Hashtbl.create 11

    let raw_llty_of_numerical (type a) (typ : a numerical) context =
      match typ with
      | I64_num -> int64_t context
      | I32_num -> int32_t context
      | I16_num -> int16_t context
      | I8_num -> int8_t context
      | F32_num -> float32_t context
      | F64_num -> float64_t context

    (* Convert type to Llvm repr *)
    let rec storage_of_type : type a. a Types.typ -> t k =
     fun (type a) (typ : a Types.typ) : t k ->
      let open K in
      let* context = context in
      match typ with
      | TNum typ -> of_numerical typ
      | TUnit -> return (bool_t context)
      | TBool -> return (bool_t context)
      | TPtr _typ -> return (Llvm.pointer_type context)
      | TArr_unk _typ -> return (Llvm.pointer_type context)
      | TArr_cst (typ, sz) ->
          if sz < 0L then (* Detected at type construction time *)
            assert false
          else
            let* lltyp = storage_of_type typ in
            return (Llvm.array_type lltyp (Int64.to_int sz))
      | TRecord record_descr -> (
          match record_descr with
          | Record_fix (id, _) -> (
              match Hashtbl.find_opt struct_table id with
              | Some ty -> return ty
              | _ ->
                  let name = Printf.sprintf "struct_%d" id in
                  let named_strct = Llvm.named_struct_type context name in
                  Hashtbl.add struct_table id named_strct ;
                  struct_of_tuple record_descr (fun fields ->
                      let packed = false in
                      Llvm.struct_set_body named_strct fields packed ;
                      return named_strct))
          | _ ->
              struct_of_tuple record_descr (fun fields ->
                  return (Llvm.struct_type context fields)))

    and of_numerical : type a. a Type_system.numerical -> t k =
     fun (typ : a Type_system.numerical) : Llvm.lltype k ->
      let open K in
      let* context = context in
      return (raw_llty_of_numerical typ context)

    and struct_of_tuple : type a b c u. (a, b, c, u) Types.record -> _ -> t k =
     fun descr k ->
      let open K in
      let rec loop : type a b c u.
          (a, b, c, u) Types.record ->
          Llvm.lltype list ->
          (Llvm.lltype array -> Llvm.lltype k) ->
          Llvm.lltype k =
       fun descr acc k ->
        match descr with
        | Types.Record_empty ->
            let fields = List.rev acc in
            k (Array.of_list fields)
        | Types.Record_field (field, rest) ->
            let* typ = storage_of_type (field_type field) in
            loop rest (typ :: acc) k
        | Types.Record_fix (id, f) ->
            let unfolded = f (seal descr) in
            assert (acc = []) ;
            assert (Hashtbl.mem struct_table id) ;
            loop unfolded acc k
      in
      loop descr [] k

    (* At function call boundaries, we must handle the fact that
       some values are passed by-ref instead of by-value. *)
    let surface_type : type a. a Types.typ -> t k =
     fun (typ : a Types.typ) : t k ->
      match typ with
      | TArr_cst _ -> storage_of_type (TPtr typ)
      | _ -> storage_of_type typ
  end

  (* helpers *)
  let create_block_after context builder block name =
    let new_block = Llvm.insert_block context name block in
    Llvm.move_block_after block new_block ;
    Llvm.position_at_end new_block builder ;
    new_block

  let append_to_insertion_block context builder name =
    let insertion_block = Llvm.insertion_block builder in
    create_block_after context builder insertion_block name

  let random_name () =
    let long_name =
      Digest.bytes (Marshal.to_bytes (Random.full_int max_int) [])
      |> Digest.to_hex
    in
    String.sub long_name 0 8

  let sf = Printf.sprintf

  module type Numerical =
    Numerical with type 'a typ := 'a typ and type 'a m = 'a m

  module type Cast = Cast with type 'a m := 'a m

  module Stack = Stack_frame (Types)

  type 's fundecl =
    { name : string; signature : 's Types.fn; fptr : Llvm.llvalue }

  let fundecl_name ({ name; _ } : _ fundecl) = name

  let unit : unit m =
    let open K in
    let* context = context in
    llreturn (Llvm.const_int (LLVM_type.bool_t context) 0) Types.unit

  let tt =
    let open K in
    let* context = context in
    llreturn (Llvm.const_int (LLVM_type.bool_t context) 1) Types.bool

  let ff =
    let open K in
    let* context = context in
    llreturn (Llvm.const_int (LLVM_type.bool_t context) 0) Types.bool

  let string ?(z = true) s =
    let open K in
    let* glob = global_string ~z s in
    let* builder = builder in
    let typ = Types.(ptr i8) in
    let* target_typ = LLVM_type.storage_of_type typ in
    llreturn (Llvm.build_bitcast glob target_typ "cast_glob" builder) typ

  let binop : type a. _ -> string -> a m -> a m -> a m =
   fun op name lhs rhs ->
    let open K in
    let* builder = builder in
    let*? lhs = lhs in
    let*? rhs = rhs in
    let typ = typeof lhs in
    let llval = op (llval lhs) (llval rhs) name builder in
    llreturn llval typ

  let bool_binop : type a. _ -> string -> a m -> a m -> bool m =
   fun op name lhs rhs ->
    let open K in
    let* builder = builder in
    let*? lhs = lhs in
    let*? rhs = rhs in
    let llval = op (llval lhs) (llval rhs) name builder in
    llreturn llval bool

  let null_ptr : 'a typ -> 'a ptr Types.m =
    let open K in
    fun ty ->
      let ptrty = Types.ptr ty in
      let* llty = LLVM_type.storage_of_type ptrty in
      let llval = Llvm.const_null llty in
      llreturn llval ptrty

  module Make_signed_int (N : sig
    type t

    type v

    val num : t numerical

    val t : t Types.typ

    val v_to_llvm : v -> Llvm.llcontext -> Llvm.llvalue

    val zero : v

    val one : v
  end) : Numerical with type t = N.t and type v = N.v = struct
    type t = N.t

    type v = N.v

    type nonrec 'a m = 'a m

    let n = N.num

    let v i =
      let open K in
      let* context = context in
      llreturn (N.v_to_llvm i context) N.t

    let zero = v N.zero

    let one = v N.one

    let s = Format.asprintf "%a" Type_system.pp_numerical N.num

    let add : t m -> t m -> t m =
     fun lhs rhs -> binop Llvm.build_add (s ^ "_add") lhs rhs

    let sub : t m -> t m -> t m =
     fun lhs rhs -> binop Llvm.build_sub (s ^ "_sub") lhs rhs

    let mul : t m -> t m -> t m =
     fun lhs rhs -> binop Llvm.build_mul (s ^ "_mul") lhs rhs

    let div : t m -> t m -> t m =
     fun lhs rhs -> binop Llvm.build_sdiv (s ^ "_div") lhs rhs

    let neg : t m -> t m = fun x -> sub zero x

    let lt : t m -> t m -> bool m =
     fun lhs rhs ->
      bool_binop (Llvm.build_icmp Llvm.Icmp.Slt) (s ^ "_lt") lhs rhs

    let le : t m -> t m -> bool m =
     fun lhs rhs ->
      bool_binop (Llvm.build_icmp Llvm.Icmp.Sle) (s ^ "_le") lhs rhs

    let eq : t m -> t m -> bool m =
     fun lhs rhs ->
      bool_binop (Llvm.build_icmp Llvm.Icmp.Eq) (s ^ "_eq") lhs rhs
  end

  module Make_float (N : sig
    type t

    val num : t numerical

    val t : t Types.typ

    val v_to_llvm : float -> Llvm.llcontext -> Llvm.llvalue
  end) : Numerical with type t = N.t and type v = float = struct
    type t = N.t

    type v = float

    type nonrec 'a m = 'a m

    let n = N.num

    let t = N.t

    let v i =
      let open K in
      let* context = context in
      llreturn (N.v_to_llvm i context) N.t

    let zero = v 0.0

    let one = v 1.0

    let s = Format.asprintf "%a" Type_system.pp_numerical N.num

    let add : t m -> t m -> t m =
     fun lhs rhs -> binop Llvm.build_fadd (s ^ "_add") lhs rhs

    let sub : t m -> t m -> t m =
     fun lhs rhs -> binop Llvm.build_fsub (s ^ "_sub") lhs rhs

    let mul : t m -> t m -> t m =
     fun lhs rhs -> binop Llvm.build_fmul (s ^ "_mul") lhs rhs

    let div : t m -> t m -> t m =
     fun lhs rhs -> binop Llvm.build_fdiv (s ^ "_div") lhs rhs

    let neg : t m -> t m =
     fun x ->
      let open K in
      let* builder = builder in
      let*? x = x in
      llreturn (Llvm.build_fneg (llval x) (s ^ "_neg") builder) t

    let lt : t m -> t m -> bool m =
     fun lhs rhs ->
      bool_binop (Llvm.build_fcmp Llvm.Fcmp.Olt) (s ^ "_lt") lhs rhs

    let le : t m -> t m -> bool m =
     fun lhs rhs ->
      bool_binop (Llvm.build_fcmp Llvm.Fcmp.Ole) (s ^ "_le") lhs rhs

    let eq : t m -> t m -> bool m =
     fun lhs rhs ->
      bool_binop (Llvm.build_fcmp Llvm.Fcmp.Oeq) (s ^ "_eq") lhs rhs
  end

  module I64 = Make_signed_int (struct
    type t = Type_system.i64

    type v = int64

    let num = I64_num

    let t = Types.i64

    let v_to_llvm v context =
      Llvm.const_int (LLVM_type.int64_t context) (Int64.to_int v)

    let zero = 0L

    let one = 1L
  end)

  module I32 = Make_signed_int (struct
    type t = Type_system.i32

    type v = int32

    let num = I32_num

    let t = Types.i32

    let v_to_llvm v context =
      Llvm.const_int (LLVM_type.int32_t context) (Int32.to_int v)

    let zero = 0l

    let one = 1l
  end)

  module I16 = Make_signed_int (struct
    type t = Type_system.i16

    type v = int

    let num = I16_num

    let t = Types.i16

    let v_to_llvm v context = Llvm.const_int (LLVM_type.int16_t context) v

    let zero = 0

    let one = 1
  end)

  module I8 = Make_signed_int (struct
    type t = Type_system.i8

    type v = char

    let num = I8_num

    let t = Types.i8

    let v_to_llvm v context =
      Llvm.const_int (LLVM_type.int8_t context) (Char.code v)

    let zero = Char.chr 0

    let one = Char.chr 1
  end)

  module F64 = Make_float (struct
    type t = Type_system.f64

    let num = F64_num

    let t = Types.f64

    let v_to_llvm f context = Llvm.const_float (LLVM_type.float64_t context) f
  end)

  module F32 = Make_float (struct
    type t = Type_system.f32

    let num = F32_num

    let t = Types.f32

    let v_to_llvm f context = Llvm.const_float (LLVM_type.float32_t context) f
  end)

  module Cast : Cast = struct
    let trunc : type a b. a numerical -> b numerical -> a m -> b m option =
      let open K in
      fun n1 n2 v ->
        match (n1, n2) with
        | (I64_num, I32_num)
        | (I64_num, I16_num)
        | (I64_num, I8_num)
        | (I32_num, I16_num)
        | (I32_num, I8_num)
        | (I16_num, I8_num) ->
            Option.some
            @@ let* builder = builder in
               let* target = LLVM_type.of_numerical n2 in
               let*? v = v in
               let truncated =
                 Llvm.build_trunc (llval v) target "trunc" builder
               in
               llreturn truncated (Types.TNum n2)
        | _ -> None

    let iext : type a b.
        (Llvm.llvalue ->
        Llvm.lltype ->
        string ->
        Llvm.llbuilder ->
        Llvm.llvalue) ->
        a numerical ->
        b numerical ->
        a m ->
        b m option =
      let open K in
      fun f n1 n2 v ->
        (* Observe how we match on [(n2, n1)] *)
        match (n2, n1) with
        | (I64_num, I32_num)
        | (I64_num, I16_num)
        | (I64_num, I8_num)
        | (I32_num, I16_num)
        | (I32_num, I8_num)
        | (I16_num, I8_num) ->
            Option.some
            @@ let* builder = builder in
               let* target = LLVM_type.of_numerical n2 in
               let*? v = v in
               let truncated = f (llval v) target "trunc" builder in
               llreturn truncated (Types.TNum n2)
        | _ -> None

    let zext : type a b. a numerical -> b numerical -> a m -> b m option =
     fun n1 n2 v -> iext Llvm.build_zext n1 n2 v

    let sext : type a b. a numerical -> b numerical -> a m -> b m option =
     fun n1 n2 v -> iext Llvm.build_sext n1 n2 v

    let f32 : type a. a numerical -> a m -> f32 m =
      let open K in
      fun n v ->
        match n with
        | I64_num | I32_num | I16_num | I8_num ->
            let* builder = builder in
            let* target = LLVM_type.of_numerical F32_num in
            let*? v = v in
            let fp = Llvm.build_sitofp (llval v) target "si_to_f32" builder in
            llreturn fp Types.f32
        | F32_num -> v
        | F64_num ->
            let* builder = builder in
            let* target = LLVM_type.of_numerical F32_num in
            let*? v = v in
            let fp = Llvm.build_fptrunc (llval v) target "f64_to_f32" builder in
            llreturn fp Types.f32

    let f64 : type a. a numerical -> a m -> f64 m =
      let open K in
      fun n v ->
        match n with
        | I64_num | I32_num | I16_num | I8_num ->
            let* builder = builder in
            let* target = LLVM_type.of_numerical F64_num in
            let*? v = v in
            let fp = Llvm.build_sitofp (llval v) target "si_to_f64" builder in
            llreturn fp Types.f64
        | F64_num -> v
        | F32_num ->
            let* builder = builder in
            let* target = LLVM_type.of_numerical F32_num in
            let*? v = v in
            let fp = Llvm.build_fpext (llval v) target "f32_to_f64" builder in
            llreturn fp Types.f64

    let of_f32 : type a. a numerical -> f32 m -> a m =
      let open K in
      fun n v ->
        match n with
        | I64_num | I32_num | I16_num | I8_num ->
            let* builder = builder in
            let* target = LLVM_type.of_numerical n in
            let*? v = v in
            let fp = Llvm.build_fptosi (llval v) target "f32_to_si" builder in
            llreturn fp (Types.TNum n)
        | F32_num -> v
        | F64_num ->
            let* builder = builder in
            let* target = LLVM_type.of_numerical F64_num in
            let*? v = v in
            let fp = Llvm.build_fpext (llval v) target "f32_to_f64" builder in
            llreturn fp Types.f64

    let of_f64 : type a. a numerical -> f64 m -> a m =
      let open K in
      fun n v ->
        match n with
        | I64_num | I32_num | I16_num | I8_num ->
            let* builder = builder in
            let* target = LLVM_type.of_numerical n in
            let*? v = v in
            let fp = Llvm.build_fptosi (llval v) target "f64_to_si" builder in
            llreturn fp (Types.TNum n)
        | F64_num -> v
        | F32_num ->
            let* builder = builder in
            let* target = LLVM_type.of_numerical F32_num in
            let*? v = v in
            let fp = Llvm.build_fptrunc (llval v) target "f64_to_f32" builder in
            llreturn fp Types.f32
  end

  let is_null : 'a ptr Types.m -> bool Types.m =
    let open K in
    fun ptr ->
      let* context = context in
      let* builder = builder in
      let*? ptr = ptr in
      let ptr =
        Llvm.build_ptrtoint
          (llval ptr)
          (LLVM_type.int64_t context)
          "ptr_to_int"
          builder
      in
      I64.eq I64.zero (llreturn ptr Types.i64)

  let ptr_eq : 'a ptr Types.m -> 'a ptr Types.m -> bool Types.m =
    let open K in
    fun ptr ptr' ->
      let* context = context in
      let* builder = builder in
      let*? ptr = ptr in
      let*? ptr' = ptr' in
      let ptr =
        Llvm.build_ptrtoint
          (llval ptr)
          (LLVM_type.int64_t context)
          "ptr_to_int"
          builder
      in
      let ptr' =
        Llvm.build_ptrtoint
          (llval ptr')
          (LLVM_type.int64_t context)
          "ptr_to_int"
          builder
      in
      I64.eq (llreturn ptr Types.i64) (llreturn ptr' Types.i64)

  let ( && ) lhs rhs = bool_binop Llvm.build_and "bool_and" lhs rhs

  let ( || ) lhs rhs = bool_binop Llvm.build_or "bool_or" lhs rhs

  let get_field : type a u. (a, u) field -> int -> u m -> a m =
   fun field index record ->
    let open K in
    (* Invariant: fixed-size arrays are passed by reference *)
    let* builder = builder in
    let*? record = record in
    let* llrecord_type =
      match typeof record with
      | TPtr record -> LLVM_type.storage_of_type record
      | _ -> assert false
    in
    let field_addr =
      let record_value = llval record in
      (* TODO: inbounds? *)
      Llvm.build_struct_gep
        llrecord_type
        record_value
        index
        ("fieldaddr_" ^ string_of_int index)
        builder
    in
    let fty = field_type field in
    let* llfty = LLVM_type.storage_of_type fty in
    match fty with
    | TArr_cst _ -> llreturn field_addr fty
    | TUnit | TBool | TNum _ | TPtr _ | TArr_unk _ | TRecord _ ->
        let v = Llvm.build_load llfty field_addr "record_get_load" builder in
        llreturn v fty

  let set_field : type a u. (a, u) field -> int -> u m -> a m -> unit m =
   fun field index record elt ->
    let open K in
    (* Invariant: fixed-size arrays are passed by reference *)
    (* TODO: add type constraints to ensure that [record] is a pointer to a record *)
    let* builder = builder in
    let*? record = record in
    let*? elt = elt in
    let* llrecord_type =
      match typeof record with
      | TPtr record -> LLVM_type.storage_of_type record
      | _ -> assert false
    in
    let field_addr =
      Llvm.build_struct_gep
        llrecord_type
        (llval record)
        index
        ("fieldaddr_" ^ string_of_int index)
        builder
    in
    let fty = field_type field in
    let* llfty = LLVM_type.storage_of_type fty in
    match fty with
    | TUnit | TBool | TNum _ | TPtr _ | TArr_unk _ | TRecord _ ->
        (* TODO: memcpy for large structs? *)
        let _ = Llvm.build_store (llval elt) field_addr builder in
        unit
    | TArr_cst (_, sz) ->
        let () =
          match typeof elt with
          | TArr_cst (_, sz') as eltty ->
              if sz <> sz' then
                Format.kasprintf
                  failwith
                  "set_field: type mismatch: field has type %a, operand has \
                   type %a"
                  Types.pp_typ
                  fty
                  Types.pp_typ
                  eltty
              else ()
          | TRecord _ -> assert false
          | _ -> .
        in
        (* TODO: memcpy for large arrays *)
        let s =
          Llvm.build_load llfty (llval elt) "record_set_arr_load" builder
        in
        let _ = Llvm.build_store s field_addr builder in
        unit

  let projs : type elim res u. (elim, _, res, u) record -> elim =
   fun record ->
    let open K in
    let rec loop : type elim acc.
        (elim, acc, res, u) Types.record -> int -> elim =
     fun (descr : (elim, acc, res, u) Types.record) index ->
      match descr with
      | Record_empty -> (() : elim)
      | Record_field ((Field _ as field), rest) ->
          let getaddr (record : u ptr m) =
            let* builder = builder in
            let*? record = record in
            let* llrecord_type =
              match typeof record with
              | TPtr record -> LLVM_type.storage_of_type record
              | _ -> assert false
            in
            let field_addr =
              Llvm.build_struct_gep
                llrecord_type
                (llval record)
                index
                ("fieldaddr_" ^ string_of_int index)
                builder
            in
            let fty = field_type field in
            llreturn field_addr (Types.ptr fty)
          in
          let proj =
            Proj
              { get = (fun r -> get_field field index r);
                getaddr;
                set = (fun r v -> set_field field index r v)
              }
          in
          let elims = loop rest (index + 1) in
          ((elims, proj) : elim)
      | Record_fix (_id, f) -> loop (f (seal descr)) index
    in
    loop record 0

  let ( .%{} ) : type a b. a ptr m -> (b, a) Types.proj -> b m =
   fun strct field -> match field with Proj { get; _ } -> get strct

  let ( .&{} ) : type a b. a ptr m -> (b, a) Types.proj -> b ptr m =
   fun strct field -> match field with Proj { getaddr; _ } -> getaddr strct

  let ( .%{}<- ) : type a b. a ptr m -> (b, a) Types.proj -> b m -> unit m =
   fun strct field v -> match field with Proj { set; _ } -> set strct v

  let seq (m : unit m) (f : unit -> 'b m) : 'b m =
    let open K in
    let* _m = m in
    f ()

  let ( let*! ) (m : 'a m) (f : 'a m -> 'b k) : 'b k =
    let open K in
    let* m = m in
    f (return m)

  let store (type a) (ptr : a ptr m) (v : a m) : unit m =
    let open K in
    let* builder = builder in
    let*? ptr = ptr in
    let*? v = v in
    match typeof ptr with
    | TPtr (TArr_cst (_, sz)) as pty ->
        let () =
          match typeof v with
          | TArr_cst (_, sz') as eltty ->
              if sz <> sz' then
                Format.kasprintf
                  failwith
                  "store: type mismatch: field has type %a, operand has type %a"
                  Types.pp_typ
                  pty
                  Types.pp_typ
                  eltty
              else ()
          | TRecord _ -> assert false
          | _ -> .
        in
        let* llvty = LLVM_type.storage_of_type (typeof v) in
        let s = Llvm.build_load llvty (llval v) "store_array_load" builder in
        let _ = Llvm.build_store s (llval ptr) builder in
        unit
    | TPtr _ ->
        let _ = Llvm.build_store (llval v) (llval ptr) builder in
        unit
    | TRecord _ -> (* can't be refuted because of abstract types *) assert false
    | _ -> .

  let load : type a. a ptr m -> a m =
   fun (ptr : a ptr m) ->
    let open K in
    let* builder = builder in
    let*? ptr = ptr in
    match typeof ptr with
    | TPtr typ -> (
        match typ with
        | TArr_cst _ -> llreturn (llval ptr) typ
        | _ ->
            let* llty = LLVM_type.storage_of_type typ in
            llreturn (Llvm.build_load llty (llval ptr) "load_tmp" builder) typ)
    | TRecord _ ->
        (* can't be refuted because of abstract types *)
        assert false
    | _ -> .

  let get_generic (type a c) (arr : (a, c) arr m) (i : I64.t m) k =
    let open K in
    let* builder = builder in
    let* context = context in
    let*? arr = arr in
    let*? i = i in
    match (typeof arr, typeof i) with
    | (TArr_unk elt_ty, TNum I64_num) ->
        let* llelt_ty = LLVM_type.storage_of_type elt_ty in
        let addr =
          Llvm.build_gep llelt_ty (llval arr) [| llval i |] "get_gep" builder
        in
        k builder addr elt_ty
    | (TArr_cst (elt_ty, _sz), TNum I64_num) ->
        let* llelt_ty = LLVM_type.storage_of_type elt_ty in
        let _zero = Llvm.const_int (LLVM_type.int64_t context) 0 in
        let addr =
          (* TODO misaligned stores/loads *)
          (* The first [zero] to account for the fact that [arr] is actually
             a pointer to the fixed-size array *)
          Llvm.build_gep
            llelt_ty
            (llval arr)
            [| (* zero; *) llval i |]
            "get_gep"
            builder
        in
        k builder addr elt_ty
    | _ -> assert false

  let get (type a c) (arr : (a, c) arr m) (i : I64.t m) : a m =
    let open K in
    let k : type a. Llvm.llbuilder -> Llvm.llvalue -> a typ -> a m =
     fun builder addr elt_ty ->
      match elt_ty with
      | TArr_cst _ ->
          (* Fixed-size arrays are mapped in LLVM to a pointer to that array.
                 The returned value has type [array t n].
                 [addr] has LLVM type `[t x n]*` where [t] is the type of elements of
                 the fixed-size array. *)
          llreturn addr elt_ty
      | _ ->
          let* llelt_ty = LLVM_type.storage_of_type elt_ty in
          let elt = Llvm.build_load llelt_ty addr "get_tmp" builder in
          llreturn elt elt_ty
    in
    get_generic arr i k

  let getaddr (type a c) (arr : (a, c) arr m) (i : I64.t m) : a ptr m =
    let open K in
    let k : type a. Llvm.llbuilder -> Llvm.llvalue -> a typ -> a ptr m =
     fun _builder addr elt_ty -> llreturn addr (Types.ptr elt_ty)
    in
    get_generic arr i k

  let set (type a c) (arr : (a, c) arr m) (i : I64.t m) (e : a m) : unit m =
    let open K in
    let* builder = builder in
    let*? arr = arr in
    let*? i = i in
    let*? e = e in
    match typeof arr with
    | TArr_unk elt_ty as aty ->
        let* llelt_ty = LLVM_type.storage_of_type elt_ty in
        (let addr =
           Llvm.build_gep llelt_ty (llval arr) [| llval i |] "set_gep" builder
         in
         match elt_ty with
         | TArr_cst (_, sz) ->
             let () =
               match typeof e with
               | TArr_cst (_, sz') as ety ->
                   if sz <> sz' then
                     Format.kasprintf
                       failwith
                       "set: type mismatch: array has type %a, element has \
                        type %a"
                       Types.pp_typ
                       aty
                       Types.pp_typ
                       ety
                   else ()
               | _ -> assert false
             in
             (* e is a pointer to a fixed-size array (LLVM type `[t x n]*`). *)
             (* TODO: for large arrays, we should issue a memcpy here *)
             let elt =
               Llvm.build_load llelt_ty (llval e) "set_load_arr" builder
             in
             ignore (Llvm.build_store elt addr builder)
         | _ -> ignore (Llvm.build_store (llval e) addr builder)) ;
        unit
    | TArr_cst (elt_ty, _sz) as aty ->
        (* [arr] is a pointer to a fixed-size array (LLVM type `[t x n]*`). *)
        let* _context = context in
        let* llelt_ty = LLVM_type.storage_of_type elt_ty in
        (* let zero = Llvm.const_int (LLVM_type.int64_t context) 0 in *)
        let addr =
          Llvm.build_gep
            llelt_ty
            (llval arr)
            [| (* zero; *) llval i |]
            "set_gep"
            builder
        in
        (match elt_ty with
        | TArr_cst (_, sz) ->
            let () =
              match typeof e with
              | TArr_cst (_, sz') as ety ->
                  if sz <> sz' then
                    Format.kasprintf
                      failwith
                      "set: type mismatch: array has type %a, element has type \
                       %a"
                      Types.pp_typ
                      aty
                      Types.pp_typ
                      ety
                  else ()
              | _ -> assert false
            in
            (* e is a pointer to a fixed-size array (LLVM type `[t x n]*`). *)
            let strct =
              Llvm.build_load llelt_ty (llval e) "set_load_arr" builder
            in
            ignore (Llvm.build_store strct addr builder)
        | _ -> ignore (Llvm.build_store (llval e) addr builder)) ;
        unit
    | TNum _ -> .
    | TRecord _ -> assert false

  let setaddr (type a c d) (arr : ((a, d) arr ptr, c) arr m) (i : I64.t m)
      (e : (a, d) arr m) : unit m =
    let open K in
    let* builder = builder in
    let*? arr = arr in
    let*? i = i in
    let*? e = e in
    match typeof arr with
    | TNum _ | TRecord _ -> assert false
    | TArr_unk elt_ty ->
        let* llelt_ty = LLVM_type.storage_of_type elt_ty in
        (let addr =
           Llvm.build_gep llelt_ty (llval arr) [| llval i |] "set_gep" builder
         in
         match elt_ty with
         | TPtr (TArr_cst _) ->
             (* e is a pointer to a fixed-size array (LLVM type `[t x n]*`). *)
             ignore (Llvm.build_store (llval e) addr builder)
         | _ ->
             failwith
               "setaddr: element type is not pointer to a struct or to a \
                fixed-sized array") ;
        unit
    | TArr_cst (elt_ty, _) ->
        (* [arr] is a pointer to a fixed-size array (LLVM type `[t x n]*`). *)
        let* _context = context in
        let* llelt_ty = LLVM_type.storage_of_type elt_ty in
        (* let zero = Llvm.const_int (LLVM_type.int64_t context) 0 in *)
        let addr =
          Llvm.build_gep
            llelt_ty
            (llval arr)
            [| (* zero; *) llval i |]
            "set_gep"
            builder
        in
        (match elt_ty with
        | TPtr (TArr_cst _) ->
            (* e is a pointer to a fixed-size array (LLVM type `[t x n]*`). *)
            ignore (Llvm.build_store (llval e) addr builder)
        | _ ->
            failwith
              "setaddr: element type is not pointer to a struct or to a \
               fixed-sized array") ;
        unit

  let ( .%[] ) arr i = get arr i

  let ( .&[] ) arr i = getaddr arr i

  let ( .%[]<- ) arr i v = set arr i v

  let ( .&[]<- ) arr i v = setaddr arr i v

  let cond (type t) (cond : bool m) (dispatch : bool -> t m) =
    let open K in
    let* builder = builder in
    let* context = context in
    let*? cond = cond in
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
    (* build conditional jump *)
    Llvm.position_at_end end_of_cond_pos builder ;
    ignore (Llvm.build_cond_br (llval cond) trueblock falseblock builder) ;
    let add_jump_to_join_from ~join ~from =
      Llvm.position_at_end from builder ;
      ignore (Llvm.build_br join builder)
    in
    match (bt, bf) with
    | (None, None) ->
        (* Both branches raise *)
        return None
    | (None, Some _bf) ->
        (* True branch raises *)
        Llvm.position_at_end falseblock' builder ;
        let phi_block = Llvm.append_block context "ifjoin" enclosing_func in
        (* insert jumps from end of the 'false branch' block to the merge node. *)
        add_jump_to_join_from ~join:phi_block ~from:falseblock' ;
        (* Move inserter at end of join block. *)
        Llvm.position_at_end phi_block builder ;
        return bf
    | (Some _bt, None) ->
        (* False branch raises *)
        Llvm.position_at_end falseblock' builder ;
        let phi_block = Llvm.append_block context "ifjoin" enclosing_func in
        (* insert jumps from end of the 'true branch' block to the merge node. *)
        add_jump_to_join_from ~join:phi_block ~from:trueblock' ;
        (* Move inserter at end of join block. *)
        Llvm.position_at_end phi_block builder ;
        return bt
    | (Some bt, Some bf) ->
        Llvm.position_at_end falseblock' builder ;
        let phi_block = Llvm.append_block context "ifjoin" enclosing_func in
        Llvm.position_at_end phi_block builder ;
        let incoming = List.[(llval bt, trueblock'); (llval bf, falseblock')] in
        let phi = Llvm.build_phi incoming "phitmp" builder in
        add_jump_to_join_from ~join:phi_block ~from:trueblock' ;
        add_jump_to_join_from ~join:phi_block ~from:falseblock' ;
        (* Move inserter at end of join block. *)
        Llvm.position_at_end phi_block builder ;
        assert (type_eq (typeof bt) (typeof bf)) ;
        llreturn phi (typeof bt)

  let for_ :
      init:I64.t m ->
      pred:(I64.t m -> bool m) ->
      step:(I64.t m -> I64.t m) ->
      (I64.t m -> unit m) ->
      unit m =
   fun ~init ~pred ~step body ->
    let open K in
    let for_name = random_name () in
    let* builder = builder in
    let* context = context in

    let current_block = Llvm.insertion_block builder in

    let for_init =
      create_block_after
        context
        builder
        current_block
        (sf "for_%s_init" for_name)
    in
    let for_entry =
      append_to_insertion_block context builder (sf "for_%s_entry" for_name)
    in
    let for_body =
      append_to_insertion_block context builder (sf "for_%s_body" for_name)
    in
    let for_exit =
      append_to_insertion_block context builder (sf "for_%s_exit" for_name)
    in

    (* Add unconditional jump from [current_block] inherited from context to [for_init] *)
    Llvm.position_at_end current_block builder ;
    let _ = Llvm.build_br for_init builder in
    (* codegen init *)
    Llvm.position_at_end for_init builder ;
    let*? init = init in
    let _ = Llvm.build_br for_entry builder in

    let last_init_block = Llvm.insertion_block builder in

    Llvm.position_at_end for_entry builder ;

    let* phi_ty = LLVM_type.storage_of_type Types.i64 in
    let phi = Llvm.build_empty_phi phi_ty (sf "for_%s_phi" for_name) builder in
    Llvm.add_incoming (llval init, last_init_block) phi ;
    let phi_expr = llreturn phi Types.i64 in
    let*? cond = pred phi_expr in
    let _ = Llvm.build_cond_br (llval cond) for_body for_exit builder in

    Llvm.position_at_end for_body builder ;
    seq (body phi_expr) @@ fun () ->
    let*? next = step phi_expr in
    let _ = Llvm.build_br for_entry builder in

    Llvm.add_incoming (llval next, Llvm.insertion_block builder) phi ;
    Llvm.position_at_end for_exit builder ;
    unit

  let foldi : type a.
      init:I64.t m ->
      acc:a m ->
      pred:(I64.t m -> a m -> bool m) ->
      step:(I64.t m -> I64.t m) ->
      (I64.t m -> a m -> a m) ->
      a m =
   fun ~init ~acc ~pred ~step body ->
    let open K in
    let foldi_name = random_name () in
    let* builder = builder in
    let* context = context in

    let current_block = Llvm.insertion_block builder in

    let foldi_init =
      create_block_after
        context
        builder
        current_block
        (sf "foldi_%s_init" foldi_name)
    in
    let foldi_entry =
      append_to_insertion_block context builder (sf "foldi_%s_entry" foldi_name)
    in
    let foldi_body =
      append_to_insertion_block context builder (sf "foldi_%s_body" foldi_name)
    in
    let foldi_exit =
      append_to_insertion_block context builder (sf "foldi_%s_exit" foldi_name)
    in

    (* Add unconditional jump from [current_block] inherited from context to [foldi_init] *)
    Llvm.position_at_end current_block builder ;
    let _ = Llvm.build_br foldi_init builder in
    (* codegen init *)
    Llvm.position_at_end foldi_init builder ;
    let*? init = init in
    let*? acc = acc in
    let _ = Llvm.build_br foldi_entry builder in

    let last_init_block = Llvm.insertion_block builder in

    Llvm.position_at_end foldi_entry builder ;

    let* counter_ty = LLVM_type.storage_of_type Types.i64 in
    let phi_counter =
      Llvm.build_empty_phi
        counter_ty
        (sf "foldi_%s_counter_phi" foldi_name)
        builder
    in
    Llvm.add_incoming (llval init, last_init_block) phi_counter ;

    let* acc_ty = LLVM_type.storage_of_type (K.typeof acc) in
    let phi_acc =
      Llvm.build_empty_phi acc_ty (sf "foldi_%s_acc_phi" foldi_name) builder
    in
    Llvm.add_incoming (llval acc, last_init_block) phi_acc ;

    let phi_counter_expr = llreturn phi_counter Types.i64 in
    let phi_acc_expr = llreturn phi_acc (K.typeof acc) in
    let*? cond = pred phi_counter_expr phi_acc_expr in
    let _ = Llvm.build_cond_br (llval cond) foldi_body foldi_exit builder in

    Llvm.position_at_end foldi_body builder ;
    let*? acc' = body phi_counter_expr phi_acc_expr in
    let*? next = step phi_counter_expr in
    let _ = Llvm.build_br foldi_entry builder in

    Llvm.add_incoming (llval next, Llvm.insertion_block builder) phi_counter ;
    Llvm.add_incoming (llval acc', Llvm.insertion_block builder) phi_acc ;
    Llvm.position_at_end foldi_exit builder ;
    phi_acc_expr

  let while_ cond body =
    let open K in
    let while_name = random_name () in
    let* builder = builder in
    let* context = context in

    let current_block = Llvm.insertion_block builder in

    let while_cond =
      append_to_insertion_block context builder (sf "cond_%s_body" while_name)
    in
    let while_body =
      append_to_insertion_block context builder (sf "while_%s_body" while_name)
    in
    let while_exit =
      append_to_insertion_block
        context
        builder
        (sf "while_%s_bodexit" while_name)
    in

    (* Insert phi at entry of [while_cond] *)
    Llvm.position_at_end while_cond builder ;
    let* phi_ty = LLVM_type.storage_of_type Types.unit in
    let phi =
      Llvm.build_empty_phi phi_ty (sf "while_%s_phi" while_name) builder
    in

    (* Add unconditional jump from [current_block] inherited from context to [while_cond] *)
    Llvm.position_at_end current_block builder ;
    let _ = Llvm.build_br while_cond builder in
    let*? u = unit in
    Llvm.add_incoming (llval u, current_block) phi ;

    (* Generate conditional, jump to body or to exit *)
    Llvm.position_at_end while_cond builder ;
    let*? cond = cond () in
    ignore (Llvm.build_cond_br (llval cond) while_body while_exit builder) ;

    (* Generate loop body, jump to cond *)
    Llvm.position_at_end while_body builder ;
    let*? _body = body () in
    ignore (Llvm.build_br while_cond builder) ;
    Llvm.add_incoming (llval u, while_body) phi ;

    Llvm.position_at_end while_exit builder ;
    unit

  let switch_i64 :
      I64.t m ->
      cases:(int64 * (unit -> 'a m)) array ->
      default:(unit -> 'a m) ->
      'a m =
   fun expr ~cases ~default ->
    let open K in
    let switch_name = random_name () in
    let* builder = builder in
    let* context = context in

    let current_block = Llvm.insertion_block builder in

    let non_default_cases = Array.length cases in

    let switch_entry =
      create_block_after
        context
        builder
        current_block
        (sf "switch_%s_entry" switch_name)
    in

    (* Add unconditional jump from [current_block] inherited from context to [switch_entry] *)
    Llvm.position_at_end current_block builder ;
    let _ = Llvm.build_br switch_entry builder in

    Llvm.position_at_end switch_entry builder ;
    (* Generate code for expr and switch, set default branch *)
    let*? expr = expr in
    let end_of_switch = Llvm.insertion_block builder in

    let cases =
      Array.mapi
        (fun i (const, case) ->
          let block =
            append_to_insertion_block
              context
              builder
              (sf "switch_%s_case_%Ld_%d" switch_name const i)
          in
          (const, case, block))
        cases
    in
    let default_block =
      append_to_insertion_block
        context
        builder
        (sf "switch_%s_case_default" switch_name)
    in
    let phi_block =
      append_to_insertion_block context builder (sf "switch_%s_phi" switch_name)
    in

    Llvm.position_at_end end_of_switch builder ;
    let switch =
      Llvm.build_switch (llval expr) default_block non_default_cases builder
    in

    (* Build default block *)
    Llvm.position_at_end default_block builder ;
    let* default = default () in
    let () =
      match default with
      | None -> ()
      | Some _ll -> ignore (Llvm.build_br phi_block builder)
    in

    let rec loop i acc =
      if i = Array.length cases then return (List.rev acc)
      else
        let (const, case, block) = cases.(i) in
        Llvm.add_case
          switch
          (Llvm.const_int (LLVM_type.int64_t context) (Int64.to_int const))
          block ;
        Llvm.position_at_end block builder ;
        let* case = case () in
        match case with
        | None -> loop (i + 1) acc
        | Some case ->
            let _ = Llvm.build_br phi_block builder in
            loop (i + 1) ((case, Llvm.insertion_block builder) :: acc)
    in

    let* non_default_cases = loop 0 [] in
    let all_cases =
      match default with
      | None -> non_default_cases
      | Some default -> (default, default_block) :: non_default_cases
    in
    match all_cases with
    | [] ->
        Llvm.delete_block phi_block ;
        llreturn_unsound
    | (first, _) :: _ ->
        Llvm.position_at_end phi_block builder ;
        let all_cases =
          List.map (fun (ll, block) -> (llval ll, block)) all_cases
        in
        let result =
          Llvm.build_phi all_cases (sf "switch_%s_phi_node" switch_name) builder
        in
        llreturn result (typeof first)

  type _ stack_allocating =
    | Local :
        'a Stack.stack_var * ('a m -> 'b stack_allocating)
        -> 'b stack_allocating
    | End_frame : 's -> 's stack_allocating

  let local var f = Local (var, f)

  let ( let*:: ) = local

  let end_frame k = End_frame k

  let rec alloca : type s. s stack_allocating -> s k =
    let open K in
    fun (type s) (frame : s stack_allocating) ->
      let* builder = builder in
      match frame with
      | End_frame k -> (K.return k : s k)
      | Local (SV_unit, rest) ->
          let* llty = LLVM_type.storage_of_type Types.unit in
          let varty = Types.(ptr unit) in
          let llalloca = Llvm.build_alloca llty "loc" builder in
          let expr = llreturn llalloca varty in
          alloca (rest expr)
      | Local (SV_bool, rest) ->
          let* llty = LLVM_type.storage_of_type Types.bool in
          let varty = Types.(ptr bool) in
          let llalloca = Llvm.build_alloca llty "loc" builder in
          let expr = llreturn llalloca varty in
          alloca (rest expr)
      | Local (SV_num n, rest) ->
          let* llty = LLVM_type.of_numerical n in
          let varty = Types.(ptr (TNum n)) in
          let llalloca = Llvm.build_alloca llty "loc" builder in
          let expr = llreturn llalloca varty in
          alloca (rest expr)
      | Local (SV_ptr ty, rest) ->
          let* llty = LLVM_type.storage_of_type (Types.ptr ty) in
          let varty = Types.(ptr (ptr ty)) in
          let llalloca = Llvm.build_alloca llty "loc" builder in
          let expr = llreturn llalloca varty in
          alloca (rest expr)
      | Local (SV_arr (ty, size), rest) -> (
          let* lltyp = LLVM_type.storage_of_type ty in
          let* size = size in
          match size with
          | None -> failwith "Suplex.alloca: size of array is unsound"
          | Some size ->
              let llalloca =
                Llvm.build_array_alloca
                  lltyp
                  (llval size)
                  "alloca_array"
                  builder
              in
              let expr = llreturn llalloca (arr ty) in
              alloca (rest expr))
      | Local (SV_arr_cst (ty, size), rest) ->
          let arr_ty = Types.arr_cst ty size in
          let* lltyp = LLVM_type.storage_of_type arr_ty in
          let llalloca = Llvm.build_alloca lltyp "alloca_cst_array" builder in
          let expr = llreturn llalloca arr_ty in
          alloca (rest expr)
      | Local (SV_strct r, rest) ->
          let varty = Types.seal r in
          let* llty = LLVM_type.storage_of_type varty in
          let llalloca = Llvm.build_alloca llty "loc" builder in
          let expr = llreturn llalloca (Types.ptr varty) in
          alloca (rest expr)

  let rec prototype : type s. s Types.fn -> Llvm.lltype list -> Llvm.lltype k =
   fun (type s) (proto : s Types.fn) acc ->
    let open K in
    match proto with
    | Types.Returning ty ->
        let* retty = LLVM_type.surface_type ty in
        return (Llvm.function_type retty (Array.of_list (List.rev acc)))
    | Types.Arrow (ty, rest) ->
        let* llty = LLVM_type.surface_type ty in
        prototype rest (llty :: acc)

  let global_array : type a v.
      (module Numerical with type t = a and type v = v) ->
      v array ->
      (a, [ `cst ]) arr m =
   fun (module N) arr ->
    let open K in
    let* ty = LLVM_type.storage_of_type (Types.TNum N.n) in
    let* mdl = lmodule in
    let* rev_elts =
      Array.fold_left
        (fun acc v ->
          let* acc = acc in
          let* v = N.v v in
          match v with
          | None ->
              (* Converting a constant should never fail. *)
              assert false
          | Some v -> return (llval v :: acc))
        (return [])
        arr
    in
    let init = Llvm.const_array ty (Array.of_list (List.rev rev_elts)) in
    let glbptr = Llvm.define_global "global_arr" init mdl in
    let suplex_ty =
      Types.arr_cst (Types.TNum N.n) (Int64.of_int (Array.length arr))
    in
    llreturn glbptr suplex_ty

  let malloc : type a. a typ -> a ptr m =
   fun typ ->
    let open K in
    let* llty = LLVM_type.storage_of_type typ in
    let* builder = builder in
    let llptr = Llvm.build_malloc llty "malloc" builder in
    llreturn llptr (Types.ptr typ)

  let malloc_array typ len =
    let open K in
    let* llty = LLVM_type.storage_of_type typ in
    let* builder = builder in
    let*? len = len in
    let llptr =
      Llvm.build_array_malloc llty (llval len) "array_malloc" builder
    in
    llreturn llptr (Types.arr typ)

  let free ptr =
    let open K in
    let* builder = builder in
    let*? ptr = ptr in
    let _llvoid = Llvm.build_free (llval ptr) builder in
    unit

  let free_array arr =
    let open K in
    let* builder = builder in
    let*? arr = arr in
    let _llvoid = Llvm.build_free (llval arr) builder in
    unit

  let rec apply_args : type s.
      s Types.fn ->
      Llvm.llvalue list ->
      s ->
      (Llvm.llvalue option, [ `Bad_arity ]) Result.t k =
   fun proto args f ->
    let open K in
    match (proto, args) with
    | (Types.Returning _, []) -> (
        let* f = f in
        match f with
        | None -> return (Ok None)
        | Some f -> return (Ok (Some f.llval)))
    | (Types.Arrow (typ, rest), arg :: args) ->
        apply_args rest args (f (llreturn arg typ))
    | _ -> return (Error `Bad_arity)

  let fundecl : type s.
      string -> s Types.fn -> (s fundecl -> s) stack_allocating -> s fundecl k =
   fun name signature body ->
    let open K in
    let* lmodule = lmodule in
    let* builder = builder in
    let* context = context in
    let signature = signature in
    let* proto = prototype signature [] in
    let fn = Llvm.declare_function name proto lmodule in
    let fundecl = { name; signature; fptr = fn } in
    let params = Llvm.params fn in
    let bb = Llvm.append_block context "entry" fn in
    Llvm.position_at_end bb builder ;
    let* f = alloca body in
    let* res_opt = apply_args signature (Array.to_list params) (f fundecl) in
    match res_opt with
    | Error `Bad_arity ->
        failwith "fundecl: LLVM function parameters do not match declared arity"
    | Ok None ->
        if not (Llvm_analysis.verify_function fn) then
          raise (Invalid_llvm_function (lmodule, fn))
        else return fundecl
    | Ok (Some res) ->
        let _ = Llvm.build_ret res builder in
        if not (Llvm_analysis.verify_function fn) then
          raise (Invalid_llvm_function (lmodule, fn))
        else return fundecl

  let call : type s. s fundecl -> s =
   fun f ->
    let open K in
    let rec loop : type s. s Types.fn -> Llvm.llvalue list k -> s =
     fun proto acc ->
      match proto with
      | Types.Returning retty ->
          let* builder = builder in
          let* acc = acc in
          let* llretty = LLVM_type.surface_type retty in
          let args = Array.of_list (List.rev acc) in
          let llfty =
            Llvm.function_type llretty (Array.map Llvm.type_of args)
          in
          let call = Llvm.build_call llfty f.fptr args "call" builder in
          llreturn call retty
      | Types.Arrow (_ty, proto) ->
          fun arg ->
            loop
              proto
              (let* arg = arg in
               match arg with
               | Some arg ->
                   let* acc = acc in
                   return (llval arg :: acc)
               | None ->
                   failwith
                     "Suplex.call: unsound expression in argument position")
    in
    loop f.signature (return [])

  let register_external : name:string -> 's Types.fn -> 's fundecl k =
   fun ~name signature ->
    let open K in
    let* lmodule = lmodule in
    let* fptr =
      match Llvm.lookup_function name lmodule with
      | None ->
          let* typ = prototype signature [] in
          let fptr = Llvm.declare_function name typ lmodule in
          if
            not
              Bool.(
                Llvm.is_externally_initialized fptr || Llvm.is_intrinsic fptr)
          then
            Format.kasprintf
              failwith
              "Suplex.register_external: unknown symbol %s"
              name ;
          return fptr
      | Some fptr -> return fptr
    in
    let fundecl = { name; signature; fptr } in
    return fundecl

  let debug_metadata = ref ""

  let set_debug_metadata : string -> unit = fun s -> debug_metadata := s

  let fail msg =
    let open K in
    let* lmodule = lmodule in
    let* builder = builder in
    let fptr =
      match Llvm.lookup_function "failwith" lmodule with
      | None -> failwith "Suplex: failwith not visible from LLVM"
      | Some fptr -> fptr
    in
    let msg =
      if String.equal !debug_metadata "" then msg
      else Format.asprintf "(%s) %s" !debug_metadata msg
    in
    let*? s = string ~z:true msg in
    let* llretty = LLVM_type.surface_type Types.unit in
    let llfty = Llvm.function_type llretty [| Llvm.type_of s.llval |] in
    let _call = Llvm.build_call llfty fptr [| s.llval |] "call" builder in
    let _unr = Llvm.build_unreachable builder in
    llreturn_unsound

  let print msg =
    let open K in
    let* lmodule = lmodule in
    let* builder = builder in
    let fptr =
      match Llvm.lookup_function "print" lmodule with
      | None -> failwith "Suplex: print not visible from LLVM"
      | Some fptr -> fptr
    in
    let*? s = string ~z:true msg in
    let* llretty = LLVM_type.surface_type Types.unit in
    let llfty = Llvm.function_type llretty [| Llvm.type_of s.llval |] in
    let _call = Llvm.build_call llfty fptr [| s.llval |] "call" builder in
    unit

  module Exec = struct
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
        ( (unit * (i64, s) proj) * ((elt, [ `unk ]) arr, s) proj,
          ((elt, [ `unk ]) arr * (i64 * unit)) Vec.t,
          ((elt, [ `unk ]) arr * (i64 * unit)) Vec.t,
          s )
        record

      val s : s typ

      val data : ((elt, [ `unk ]) arr, s) proj

      val dim : (i64, s) proj
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
      open Types

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

      let elt_ty = Types.TNum numty

      let r = empty_rec |+ field "dim" i64 |+ field "data" (arr elt_ty)

      let s : s typ = seal r

      let (((), dim), data) = projs r

      let ctypes_elt_ty : c_elt Ctypes.typ =
        match X.rel with
        | I64_rel -> Ctypes.int64_t
        | I32_rel -> Ctypes.int32_t
        | I16_rel -> Ctypes.int16_t
        | I8_rel -> Ctypes.int8_t
        | F32_rel -> Ctypes.float
        | F64_rel -> Ctypes.double

      let ((ctypes : c Ctypes_static.structure Ctypes_static.typ), cdim, cdata)
          =
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

    include (
      struct
        type 'a opaque = unit Ctypes.ptr

        let opaque = Ctypes.(ptr void)
      end :
        sig
          type 'a opaque

          val opaque : 'a opaque Ctypes.typ
        end)

    type (_, _, _) full_rel =
      | Full_rel_unit : (unit m, unit, bool) full_rel
      | Full_rel_bool : (bool m, bool, bool) full_rel
      | Full_rel_int64 : (I64.t m, int64, int64) full_rel
      | Full_rel_int32 : (I32.t m, int32, int32) full_rel
      | Full_rel_int16 : (I16.t m, int, int) full_rel
      | Full_rel_int8 : (I8.t m, int, int) full_rel
      | Full_rel_float64 : (F64.t m, float, float) full_rel
      | Full_rel_float32 : (F32.t m, float, float) full_rel
      | Full_rel_string : (I8.t ptr m, string, string) full_rel
      | Full_rel_ba_i64 :
          ( I64_ba.s ptr m,
            (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t,
            I64_ba.c Ctypes.structure Ctypes.ptr )
          full_rel
      | Full_rel_ba_i32 :
          ( I32_ba.s ptr m,
            (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t,
            I32_ba.c Ctypes.structure Ctypes.ptr )
          full_rel
      | Full_rel_ba_i16 :
          ( I16_ba.s ptr m,
            ( int,
              Bigarray.int16_signed_elt,
              Bigarray.c_layout )
            Bigarray.Array1.t,
            I16_ba.c Ctypes.structure Ctypes.ptr )
          full_rel
      | Full_rel_ba_i8 :
          ( I8_ba.s ptr m,
            (int, Bigarray.int8_signed_elt, Bigarray.c_layout) Bigarray.Array1.t,
            I8_ba.c Ctypes.structure Ctypes.ptr )
          full_rel
      | Full_rel_ba_f64 :
          ( F64_ba.s ptr m,
            (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t,
            F64_ba.c Ctypes.structure Ctypes.ptr )
          full_rel
      | Full_rel_ba_f32 :
          ( F32_ba.s ptr m,
            (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t,
            F32_ba.c Ctypes.structure Ctypes.ptr )
          full_rel
      | Full_rel_arr_unk :
          ('suplex m, 'ocaml, 'ctypes) full_rel
          -> ( ('suplex, [ `unk ]) arr m,
               'ocaml Seq.t,
               'ctypes Ctypes.ptr )
             full_rel
      | Full_rel_arr_cst :
          int * ('suplex m, 'ocaml, 'ctypes) full_rel
          -> ( ('suplex, [ `cst ]) arr m,
               'ocaml Seq.t,
               'ctypes Ctypes.carray )
             full_rel
      | Full_rel_arr_cst_toplevel :
          int * ('suplex m, 'ocaml, 'ctypes) full_rel
          -> ( ('suplex, [ `cst ]) arr m,
               'ocaml Seq.t,
               'ctypes Ctypes.carray Ctypes.ptr )
             full_rel
          (** [Full_rel_arr_cst_toplevel] is not directly visible to the user.
              We silently replace [Full_rel_arr_cst] by this constructor to
              honor the fact that arrays are passed by-ref and not by-value *)
      | Full_rel_malloced_struct :
          (_, 'suplex Vec.t, 'suplex Vec.t, 'u) Types.record
          * ('suplex Vec.t, 'ocaml Vec.t, 'ctypes Vec.t) full_rel_vec
          -> ( 'u ptr m,
               'ocaml Vec.t,
               'u Ctypes.structure Ctypes_static.ptr )
             full_rel
      | Full_rel_opaque_malloced_struct :
          (_, 'suplex Vec.t, 'suplex Vec.t, 'u) Types.record
          -> ('u ptr m, 'u opaque, 'u opaque) full_rel
      | Full_rel_struct :
          (_, 'suplex Vec.t, 'suplex Vec.t, 'u) Types.record
          * ('suplex Vec.t, 'ocaml Vec.t, 'ctypes Vec.t) full_rel_vec
          -> ('u m, 'ocaml Vec.t, 'u Ctypes.structure) full_rel

    and (_, _, _) full_rel_vec =
      | [] : (unit Vec.t, unit Vec.t, unit Vec.t) full_rel_vec
      | ( :: ) :
          ('suplex m, 'ocaml, 'ctypes) full_rel
          * ('stail Vec.t, 'otail Vec.t, 'ctail Vec.t) full_rel_vec
          -> ( ('suplex * 'stail) Vec.t,
               ('ocaml * 'otail) Vec.t,
               ('ctypes * 'ctail) Vec.t )
             full_rel_vec

    type (_, _, _) full_rel_fn =
      | Fn_returning :
          ('suplex m, 'ocaml, 'ctypes) full_rel
          -> ('suplex m, 'ocaml, 'ctypes Ctypes.fn) full_rel_fn
      | Fn_arrow :
          ('sdom m, 'odom, 'cdom) full_rel
          * ('srange, 'orange, 'crange Ctypes.fn) full_rel_fn
          -> ( 'sdom m -> 'srange,
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

    let empty = Vec []

    let ( |+ ) (Vec v) (Rel r) = Vec (r :: v)

    let unit = Rel Full_rel_unit

    let bool = Rel Full_rel_bool

    let i64 = Rel Full_rel_int64

    let i32 = Rel Full_rel_int32

    let i16 = Rel Full_rel_int16

    let i8 = Rel Full_rel_int8

    let f64 = Rel Full_rel_float64

    let f32 = Rel Full_rel_float32

    let bigarray_i64 = Rel Full_rel_ba_i64

    let bigarray_i32 = Rel Full_rel_ba_i32

    let bigarray_i16 = Rel Full_rel_ba_i16

    let bigarray_i8 = Rel Full_rel_ba_i8

    let bigarray_f64 = Rel Full_rel_ba_f64

    let bigarray_f32 = Rel Full_rel_ba_f32

    let string = Rel Full_rel_string

    let array_raw (Rel r) = Rel (Full_rel_arr_unk r)

    let array len (Rel r) = Rel (Full_rel_arr_cst (len, r))

    let mallocd_strct strct (Vec spec) =
      Rel (Full_rel_malloced_struct (strct, spec))

    let opaque_mallocd_strct strct = Rel (Full_rel_opaque_malloced_struct strct)

    let strct strct (Vec spec) = Rel (Full_rel_struct (strct, spec))

    let returning (Rel r) = Fn (Fn_returning r)

    let subst : type a b. (a, b) rel -> (a, b) rel =
     fun (Rel r as full) ->
      match r with
      | Full_rel_arr_cst (len, rel) ->
          Rel (Full_rel_arr_cst_toplevel (len, rel))
      | _ -> full

    let ( @-> ) dom (Fn range) =
      let (Rel dom) = subst dom in
      Fn (Fn_arrow (dom, range))

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

    let bigarray_to_cstruct (type elt c_elt c)
        ((module BA) : (elt, c_elt, c) ba) v =
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
            "ctypes_to_ocaml: can't convert an array with unknown length to \
             OCaml"
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
          (s Vec.t, o' Vec.t, c) full_rel_vec ->
          (o' Vec.t -> o Vec.t) ->
          o Vec.t =
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

    let rec box_ctypes_fn : type s o c.
        (s, o, c Ctypes.fn) full_rel_fn -> c -> o =
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

    let rec extract_suplex : type s o c. (s m, o, c) full_rel -> s Types.typ =
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

    let rec prototype_of_rel : type s o c. (s, o, c) full_rel_fn -> s Types.fn =
     fun rel ->
      match rel with
      | Fn_returning r -> Types.returning (extract_suplex r)
      | Fn_arrow (dom, range) ->
          Types.(extract_suplex dom @-> prototype_of_rel range)

    type cfg = Llvm_executionengine.llcompileroptions

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

    let add_fdecl fdecl rel mdl =
      Add_fundecl { fdecl = Fundecl { fdecl; rel }; mdl }

    let rec lookup_function : type s f.
        s module_ -> string -> f Types.fn -> f fundecl option =
     fun mdl name proto ->
      match mdl with
      | Empty_module -> None
      | Add_fundecl { fdecl = Fundecl { fdecl; rel = _ }; mdl } ->
          if
            Bool.(
              String.equal name fdecl.name && Types.fn_eq fdecl.signature proto)
          then
            (* We resort to an [Obj.magic] here which is bad style. A cleaner way
               would be to have [Types.fn_eq] return a [('a, 'b) eq option]. However,
               due to our heavy use of GADTs in the type system this is not feasible.
            *)
            Some (Obj.magic fdecl)
          else lookup_function mdl name proto

    let run_module : type s. ?cfg:cfg -> s module_ k -> s =
     fun ?(cfg = Llvm_executionengine.default_compiler_options) mdl ->
      let (state, mdl) =
        try K.run (K.default_cfg ()) mdl
        with Invalid_llvm_function (m, _) as e ->
          Llvm.dump_module m ;
          raise e
      in
      let engine = Llvm_executionengine.create ~options:cfg state.llvm_module in
      Llvm.print_module "debug.ll" state.llvm_module ;
      (* let fpm = Llvm.PassManager.create () in *)
      (* ignore (Llvm.PassManager.run_module state.llvm_module fpm) ; *)
      let roots = ref List.[] in
      let rec loop : type s. s module_ -> s =
       fun mdl ->
        match mdl with
        | Empty_module -> ()
        | Add_fundecl { fdecl = Fundecl { fdecl; rel = Fn rel }; mdl } -> (
            let rest = loop mdl in
            match rel with
            | Fn_returning _ ->
                (* Cannot be refuted because of abstract bigarray type *)
                assert false
            | Fn_arrow (_, _) ->
                let fn_ptr_typ = Foreign.funptr (extract_ctypes_fn rel) in
                let f =
                  Llvm_executionengine.get_function_address
                    (fundecl_name fdecl)
                    fn_ptr_typ
                    engine
                in
                let f = box_ctypes_fn rel f in
                roots := Obj.magic f :: !roots ;
                (rest, f))
      in
      let res = loop mdl in
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

    let run_program (type s tdom trange) ?cfg (fdecl : s fundecl k)
        (rel : (s, tdom -> trange) fn_rel) : tdom -> trange =
      let open K in
      let ((), res) =
        run_module
          ?cfg
          (let* fdecl = fdecl in
           return
             (Add_fundecl { fdecl = Fundecl { fdecl; rel }; mdl = Empty_module }))
      in
      res

    let run ?cfg ?(fname = "dummy") (Fn rel as fn_rel) body =
      run_program ?cfg (fundecl fname (prototype_of_rel rel) body) fn_rel
  end
end

include Impl

module Externals = struct
  external fail : unit -> unit = "failwith"
end
