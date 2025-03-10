open Types
open Syntax

let sf = Printf.sprintf

let base_numerical_of_num_rel : type s o. (s, o) num_rel -> s base_numerical =
 fun rel ->
  match rel with
  | I64_rel -> I64_num
  | I32_rel -> I32_num
  | I16_rel -> I16_num
  | I8_rel -> I8_num
  | F64_rel -> F64_num
  | F32_rel -> F32_num

let numerical_of_num_rel : type s o. (s, o) num_rel -> s numerical =
 fun rel -> Base_num (base_numerical_of_num_rel rel)

let cast_base_vector_type : type a b sz.
    (a, sz) vec numerical -> b base_numerical -> (b, sz) vec numerical =
 fun (numty : (a, sz) vec numerical) (base : b base_numerical) ->
  match numty with Vec_num { base = _; numel } -> Vec_num { base; numel }

let numerical_of_type : type a sz. (a, sz) vec typ -> (a, sz) vec numerical =
 fun (typ : (a, sz) vec typ) -> match typ with TNum numty -> numty

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

  let raw_llty_of_base_numerical (type a) (typ : a base_numerical) context =
    match typ with
    | I64_num -> int64_t context
    | I32_num -> int32_t context
    | I16_num -> int16_t context
    | I8_num -> int8_t context
    | F32_num -> float32_t context
    | F64_num -> float64_t context

  let raw_llty_of_numerical (type a) (typ : a numerical) context =
    match typ with
    | Base_num b -> raw_llty_of_base_numerical b context
    | Vec_num { base; numel } ->
        let base = raw_llty_of_base_numerical base context in
        Llvm.vector_type base (Size.to_int numel)

  (* Convert type to Llvm repr *)
  let rec storage_of_type : type a. Llvm.llcontext -> a typ -> t =
   fun (type a) context (typ : a typ) ->
    match typ with
    | TNum numty -> of_numerical context numty
    | TUnit -> bool_t context
    | TBool -> bool_t context
    | TPtr _typ -> Llvm.pointer_type context
    | TArr_unk _typ -> Llvm.pointer_type context
    | TArr_cst (typ, sz) ->
        assert (sz >= 0L) ;
        let lltyp = storage_of_type context typ in
        Llvm.array_type lltyp (Int64.to_int sz)
    | TRecord { descr = record_descr } -> (
        match record_descr with
        | Record_fix (id, _) -> (
            match Hashtbl.find_opt struct_table id with
            | Some ty -> ty
            | _ ->
                let name = sf "struct_%d" id in
                let named_strct = Llvm.named_struct_type context name in
                Hashtbl.add struct_table id named_strct ;
                let fields = struct_of_tuple context record_descr in
                let packed = false in
                Llvm.struct_set_body named_strct fields packed ;
                named_strct)
        | _ ->
            let fields = struct_of_tuple context record_descr in
            Llvm.struct_type context fields)
    | TFn _ -> Llvm.pointer_type context

  and of_numerical : type a. Llvm.llcontext -> a numerical -> t =
   fun context (typ : a numerical) : Llvm.lltype ->
    raw_llty_of_numerical typ context

  and struct_of_tuple : type a b c u.
      Llvm.llcontext -> (a, b, c, u) record_desc -> t array =
   fun context descr ->
    let rec loop : type a b c u.
        (a, b, c, u) record_desc -> Llvm.lltype list -> Llvm.lltype array =
     fun descr acc ->
      match descr with
      | Record_empty -> Array.of_list acc
      | Record_field { field; index = _; rest } ->
          let typ = storage_of_type context (Types.field_type field) in
          loop rest (typ :: acc)
      | Record_fix (id, f) ->
          let unfolded = f (Types.seal descr) in
          assert (acc = []) ;
          assert (Hashtbl.mem struct_table id) ;
          loop unfolded acc
    in
    loop descr []

  (* At function call boundaries, we must handle the fact that
       some values are passed by-ref instead of by-value. *)
  let surface_type : type a. Llvm.llcontext -> a typ -> t =
   fun context (typ : a typ) : t ->
    match typ with
    | TArr_cst _ -> storage_of_type context (TPtr typ)
    | TRecord _ -> storage_of_type context (TPtr typ)
    | _ -> storage_of_type context typ
end

let field_name : type a u. (a, u record) field -> string =
 fun (type a u) (field : (a, u record) field) ->
  match field with Field { name; _ } -> name

let is_floating_point : type a. a numerical -> bool =
 fun (type a) (typ : a numerical) ->
  match numerical_kind typ with `fp -> true | `int -> false

module SMap = Map.Make (String)

type llvm_state =
  { llvm_context : Llvm.llcontext;
    llvm_module : Llvm.llmodule;
    llvm_builder : Llvm.llbuilder Stdlib.Stack.t;
    externals : (string, Llvm.llvalue) Hashtbl.t;
    const_strings : (string, Llvm.llvalue) Hashtbl.t
  }

let save_builder llvm_state f =
  let builder = Llvm.builder llvm_state.llvm_context in
  Stdlib.Stack.push builder llvm_state.llvm_builder ;
  let res = f () in
  let _ = Stdlib.Stack.pop llvm_state.llvm_builder in
  res

let get_builder llvm_state = Stdlib.Stack.top llvm_state.llvm_builder

let new_llvm_state () =
  let llvm_context = Llvm.global_context () in
  let llvm_module = Llvm.create_module llvm_context "jit" in
  let externals = Hashtbl.create 11 in
  let const_strings = Hashtbl.create 11 in
  (* builtins *)
  let typ =
    Llvm.function_type
      (Llvm.i1_type llvm_context)
      [| Llvm.pointer_type llvm_context |]
  in
  ignore (Llvm.declare_function "failwith" typ llvm_module) ;
  ignore (Llvm.declare_function "print" typ llvm_module) ;
  { llvm_context;
    llvm_module;
    llvm_builder = Stdlib.Stack.create ();
    externals;
    const_strings
  }

let with_type ty value = Some { value; ty }

let global_string (state : llvm_state) ~z s =
  match Hashtbl.find_opt state.const_strings s with
  | Some llglob ->
      (* llglob is a fixed-size array! *)
      llglob
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
      llglob

let string (state : llvm_state) ?(z = true) s =
  let glob = global_string state ~z s in
  let typ = Types.(ptr i8) in
  let target_typ = LLVM_type.storage_of_type state.llvm_context typ in
  with_type
    typ
    (Llvm.build_bitcast glob target_typ "cast_glob" (get_builder state))

let create_block_after state block name =
  let new_block = Llvm.insert_block state.llvm_context name block in
  Llvm.move_block_after block new_block ;
  Llvm.position_at_end new_block (get_builder state) ;
  new_block

let append_to_insertion_block state name =
  let insertion_block = Llvm.insertion_block (get_builder state) in
  create_block_after state insertion_block name

let random_name () =
  let long_name =
    Digest.bytes (Marshal.to_bytes (Random.full_int max_int) [])
    |> Digest.to_hex
  in
  String.sub long_name 0 8

let ( let* ) = Option.bind

let return_unit state =
  with_type Types.unit @@ Llvm.const_int (LLVM_type.bool_t state.llvm_context) 0

let get_field_addr : type a u.
    llvm_state ->
    (a, u record) field ->
    u record typ ->
    u record typed_llvm ->
    a ptr typed_llvm option =
 fun state field recty record_ptr ->
  match Types.field_index field recty with
  | Error msg -> failwith msg
  | Ok None ->
      Format.kasprintf
        failwith
        "GetFieldAddr: field %a not found"
        Types.pp_field
        field
  | Ok (Some index) ->
      let llrecord_type = LLVM_type.storage_of_type state.llvm_context recty in
      let field_addr =
        Llvm.build_struct_gep
          llrecord_type
          record_ptr.value
          index
          (sf "gep_get_field_addr_%s_%d" (field_name field) index)
          (get_builder state)
      in
      let fty = field_type field in
      with_type (Types.ptr fty) field_addr

let get_field : type a u.
    llvm_state ->
    (a, u record) field ->
    u record typ ->
    u record typed_llvm ->
    a typed_llvm option =
 fun state field recty record_ptr ->
  let* field_addr = get_field_addr state field recty record_ptr in
  let fty = field_type field in
  let llfty = LLVM_type.storage_of_type state.llvm_context fty in
  match fty with
  | TArr_cst _ | TRecord _ -> with_type fty field_addr.value
  | TUnit | TBool | TNum _ | TPtr _ | TArr_unk _ | TFn _ ->
      let v =
        Llvm.build_load
          llfty
          field_addr.value
          (sf "load_get_field_%s" (field_name field))
          (get_builder state)
      in
      with_type fty v

let set_field : type a u.
    llvm_state ->
    (a, u record) field ->
    u record typ ->
    u record typed_llvm ->
    a typed_llvm ->
    unit typed_llvm option =
 fun state field recty record_ptr v ->
  let* field_addr = get_field_addr state field recty record_ptr in
  let fty = field_type field in
  let llfty = LLVM_type.storage_of_type state.llvm_context fty in
  let perform_copy instr_name =
    let v_arr = Llvm.build_load llfty v.value instr_name (get_builder state) in
    let _ = Llvm.build_store v_arr field_addr.value (get_builder state) in
    return_unit state
  in
  match fty with
  | TArr_cst (_, sz) ->
      Types.assert_const_array v.ty ~expected_size:sz ;
      perform_copy "set_field_arr_load"
  | TRecord _ -> perform_copy "set_field_record_load"
  | TUnit | TBool | TNum _ | TPtr _ | TArr_unk _ | TFn _ ->
      let _ = Llvm.build_store v.value field_addr.value (get_builder state) in
      return_unit state

let with_extended_env env bound f =
  let key = Hmap.Key.create () in
  let env = Hmap.add key bound env in
  f env (Var key)

exception Invalid_llvm_function of Llvm.llmodule * Llvm.llvalue

let const : type s o. (s, o) num_rel -> o -> s expr =
 fun rel v ->
  match rel with
  | I64_rel -> I64 v
  | I32_rel -> I32 v
  | I16_rel -> I16 v
  | I8_rel -> I8 v
  | F64_rel -> F64 v
  | F32_rel -> F32 v

(* Invariant: expression of type array_cst is a pointer to the array on the LLVM side *)

let compile_const : type s o.
    Llvm.llcontext -> (s, o) num_rel -> o -> s typed_llvm option =
 fun llvm_context rel v ->
  match rel with
  | I64_rel ->
      with_type Types.i64
      @@ Llvm.const_int (LLVM_type.int64_t llvm_context) (Int64.to_int v)
  | I32_rel ->
      with_type Types.i32
      @@ Llvm.const_int (LLVM_type.int32_t llvm_context) (Int32.to_int v)
  | I16_rel ->
      with_type Types.i16 @@ Llvm.const_int (LLVM_type.int16_t llvm_context) v
  | I8_rel ->
      with_type Types.i8 @@ Llvm.const_int (LLVM_type.int8_t llvm_context) v
  | F64_rel ->
      with_type Types.f64
      @@ Llvm.const_float (LLVM_type.float64_t llvm_context) v
  | F32_rel ->
      with_type Types.f32
      @@ Llvm.const_float (LLVM_type.float32_t llvm_context) v

let rec compile : type a.
    environment -> llvm_state -> a expr -> a typed_llvm option =
 fun env state expr ->
  match expr with
  | Var v -> (
      match Hmap.find v env with
      | None -> Format.kasprintf failwith "Variable not found"
      | Some _ as res -> res)
  | Let (bound, body) ->
      let* bound = compile env state bound in
      with_extended_env env bound (fun env bound ->
          compile env state (body bound))
  | Unit -> return_unit state
  | True ->
      with_type Types.bool
      @@ Llvm.const_int (LLVM_type.bool_t state.llvm_context) 1
  | False ->
      with_type Types.bool
      @@ Llvm.const_int (LLVM_type.bool_t state.llvm_context) 0
  | String { strz; str } ->
      let glob = global_string state ~z:strz str in
      let typ = Types.(ptr i8) in
      let target_typ = LLVM_type.storage_of_type state.llvm_context typ in
      with_type typ
      @@ Llvm.build_bitcast glob target_typ "cast_glob" (get_builder state)
  | And (l, r) ->
      let* l = compile env state l in
      let* r = compile env state r in
      with_type Types.bool
      @@ Llvm.build_and l.value r.value "bool_and" (get_builder state)
  | Or (l, r) ->
      let* l = compile env state l in
      let* r = compile env state r in
      with_type Types.bool
      @@ Llvm.build_or l.value r.value "bool_or" (get_builder state)
  | I64 i -> compile_const state.llvm_context I64_rel i
  | I32 i -> compile_const state.llvm_context I32_rel i
  | I16 i -> compile_const state.llvm_context I16_rel i
  | I8 i -> compile_const state.llvm_context I8_rel i
  | F64 f -> compile_const state.llvm_context F64_rel f
  | F32 f -> compile_const state.llvm_context F32_rel f
  | Vec (num_rel, sz, values) ->
      if Size.to_int sz <> Array.length values then
        Format.kasprintf
          failwith
          "When constructing vector: vector of values has length %d, expected \
           %d"
          (Array.length values)
          (Size.to_int sz) ;
      let values =
        try
          Array.map
            (fun v ->
              match compile_const state.llvm_context num_rel v with
              | None -> raise Exit
              | Some v -> v.value)
            values
        with Exit ->
          Format.kasprintf
            failwith
            "When constructing vector: could not compile constant"
      in
      with_type
        (Types.vec (base_numerical_of_num_rel num_rel) sz)
        (Llvm.const_vector values)
  | Const_array (numrel, arr) ->
      let ty =
        LLVM_type.storage_of_type
          state.llvm_context
          (TNum (numerical_of_num_rel numrel))
      in
      let elts =
        Array.map
          (fun v ->
            (* Converting a constant should never fail *)
            let v_opt = compile_const state.llvm_context numrel v in
            let v = match v_opt with None -> assert false | Some v -> v in
            v.value)
          arr
      in
      let init = Llvm.const_array ty elts in
      let glbptr = Llvm.define_global "global_arr" init state.llvm_module in
      let arr_ty =
        Types.arr_cst
          (TNum (numerical_of_num_rel numrel))
          (Int64.of_int (Array.length arr))
      in
      with_type arr_ty glbptr
  | Add (numty, l, r) ->
      let* l = compile env state l in
      let* r = compile env state r in
      let opname = Format.asprintf "%a_add" pp_numerical numty in
      let instr =
        if is_floating_point numty then
          Llvm.build_fadd l.value r.value opname (get_builder state)
        else Llvm.build_add l.value r.value opname (get_builder state)
      in
      with_type (TNum numty) instr
  | Sub (numty, l, r) ->
      let* l = compile env state l in
      let* r = compile env state r in
      let opname = Format.asprintf "%a_sub" pp_numerical numty in
      let instr =
        if is_floating_point numty then
          Llvm.build_fadd l.value r.value opname (get_builder state)
        else Llvm.build_add l.value r.value opname (get_builder state)
      in
      with_type (TNum numty) instr
  | Mul (numty, l, r) ->
      let* l = compile env state l in
      let* r = compile env state r in
      let opname = Format.asprintf "%a_mul" pp_numerical numty in
      let instr =
        if is_floating_point numty then
          Llvm.build_fmul l.value r.value opname (get_builder state)
        else Llvm.build_mul l.value r.value opname (get_builder state)
      in
      with_type (TNum numty) instr
  | Div (numty, l, r) ->
      let* l = compile env state l in
      let* r = compile env state r in
      let opname = Format.asprintf "%a_div" pp_numerical numty in
      let instr =
        if is_floating_point numty then
          Llvm.build_fdiv l.value r.value opname (get_builder state)
        else Llvm.build_sdiv l.value r.value opname (get_builder state)
      in
      with_type (TNum numty) instr
  | Neg (numty, e) ->
      let* e = compile env state e in
      let opname = Format.asprintf "%a_neg" pp_numerical numty in
      with_type (TNum numty)
      @@ Llvm.build_neg e.value opname (get_builder state)
  | Lt (numty, l, r) ->
      let* l = compile env state l in
      let* r = compile env state r in
      let opname = Format.asprintf "%a_lt" pp_numerical numty in
      let instr =
        if is_floating_point numty then
          Llvm.build_fcmp
            Llvm.Fcmp.Olt
            l.value
            r.value
            opname
            (get_builder state)
        else
          Llvm.build_icmp
            Llvm.Icmp.Slt
            l.value
            r.value
            opname
            (get_builder state)
      in
      with_type Types.bool instr
  | Le (numty, l, r) ->
      let* l = compile env state l in
      let* r = compile env state r in
      let opname = Format.asprintf "%a_le" pp_numerical numty in
      let instr =
        if is_floating_point numty then
          Llvm.build_fcmp
            Llvm.Fcmp.Ole
            l.value
            r.value
            opname
            (get_builder state)
        else
          Llvm.build_icmp
            Llvm.Icmp.Sle
            l.value
            r.value
            opname
            (get_builder state)
      in
      with_type Types.bool instr
  | Eq (numty, l, r) ->
      let* l = compile env state l in
      let* r = compile env state r in
      let opname = Format.asprintf "%a_eq" pp_numerical numty in
      let instr =
        if is_floating_point numty then
          Llvm.build_fcmp
            Llvm.Fcmp.Oeq
            l.value
            r.value
            opname
            (get_builder state)
        else
          Llvm.build_icmp
            Llvm.Icmp.Eq
            l.value
            r.value
            opname
            (get_builder state)
      in
      with_type Types.bool instr
  | PtrEq (l, r) ->
      let* l = compile env state l in
      let* r = compile env state r in
      let lptr =
        Llvm.build_ptrtoint
          l.value
          (LLVM_type.int64_t state.llvm_context)
          "ptr_to_int"
          (get_builder state)
      in
      let rptr =
        Llvm.build_ptrtoint
          r.value
          (LLVM_type.int64_t state.llvm_context)
          "ptr_to_int"
          (get_builder state)
      in
      let opname = "ptr_eq" in
      with_type Types.bool
      @@ Llvm.build_icmp Llvm.Icmp.Eq lptr rptr opname (get_builder state)
  | Store (ptr, v) -> (
      let* ptr = compile env state ptr in
      let* v = compile env state v in
      let perform_copy instr_name =
        let llvty = LLVM_type.storage_of_type state.llvm_context v.ty in
        let s = Llvm.build_load llvty v.value instr_name (get_builder state) in
        let _ = Llvm.build_store s ptr.value (get_builder state) in
        return_unit state
      in
      match ptr.ty with
      | TPtr (TArr_cst (_, sz1)) ->
          Types.assert_const_array v.ty ~expected_size:sz1 ;
          perform_copy "store_array_load"
      | TPtr (TRecord _) -> perform_copy "store_record_load"
      | TPtr _ ->
          let _ = Llvm.build_store v.value ptr.value (get_builder state) in
          return_unit state
      | TNum (Base_num _) -> .)
  | Load ptr -> (
      let* ptr = compile env state ptr in
      match ptr.ty with
      | TNum (Base_num _) -> .
      | TPtr typ -> (
          match typ with
          | TArr_cst _ | TRecord _ -> with_type typ ptr.value
          | _ ->
              let llty = LLVM_type.storage_of_type state.llvm_context typ in
              with_type typ
              @@ Llvm.build_load llty ptr.value "load_tmp" (get_builder state)))
  | NullPtr ty ->
      let ptrty = Types.ptr ty in
      let llty = LLVM_type.storage_of_type state.llvm_context ptrty in
      with_type ptrty (Llvm.const_null llty)
  | IsNull ptr ->
      let* ptr = compile env state ptr in
      let ptr =
        Llvm.build_ptrtoint
          ptr.value
          (LLVM_type.int64_t state.llvm_context)
          "ptr_to_int"
          (get_builder state)
      in
      let* zero = compile env state (I64 0L) in
      let res =
        Llvm.build_icmp Llvm.Icmp.Eq ptr zero.value "isnull" (get_builder state)
      in
      with_type Types.bool res
  | AddrOf (addressable, e) -> (
      let* e = compile env state e in
      match addressable with
      | Addressable_array -> (
          match e.ty with
          | TNum (Base_num _) -> .
          | TArr_cst (_, _) -> with_type (Types.ptr e.ty) e.value)
      | Addressable_record -> (
          match e.ty with
          | TNum (Base_num _) -> .
          | TRecord _ -> with_type (Types.ptr e.ty) e.value))
  | Get (arr, i) ->
      get_generic env state arr i (fun addr elt_ty ->
          match elt_ty with
          | TArr_cst _ | TRecord _ -> with_type elt_ty addr
          | _ ->
              let lletl_ty =
                LLVM_type.storage_of_type state.llvm_context elt_ty
              in
              let elt =
                Llvm.build_load lletl_ty addr "get_tmp" (get_builder state)
              in
              with_type elt_ty elt)
  | GetAddr (arr, i) ->
      get_generic env state arr i (fun addr elt_ty ->
          with_type (Types.ptr elt_ty) addr)
  | Set (arr, i, v) -> (
      let* arr = compile env state arr in
      let* i = compile env state i in
      let* v = compile env state v in
      match arr.ty with
      | TNum (Base_num _) -> .
      | TArr_cst (elt_ty, _) | TArr_unk elt_ty -> (
          let llelt_ty = LLVM_type.storage_of_type state.llvm_context elt_ty in
          let elt_addr =
            Llvm.build_gep
              llelt_ty
              arr.value
              [| (* zero; *) i.value |]
              "set_gep"
              (get_builder state)
          in
          let perform_copy instr_name =
            let v_arr =
              Llvm.build_load llelt_ty v.value instr_name (get_builder state)
            in
            ignore @@ Llvm.build_store v_arr elt_addr (get_builder state) ;
            return_unit state
          in
          match elt_ty with
          | TArr_cst (_, sz) ->
              Types.assert_const_array v.ty ~expected_size:sz ;
              perform_copy "set_load_arr"
          | TRecord _ -> perform_copy "set_load_record"
          | _ ->
              ignore @@ Llvm.build_store v.value elt_addr (get_builder state) ;
              return_unit state))
  | GetField (field, record_ptr) -> (
      let* record_ptr = compile env state record_ptr in
      match record_ptr.ty with
      | TNum (Base_num _) -> .
      | TRecord _ as recty -> get_field state field recty record_ptr)
  | GetFieldAddr (field, record_ptr) -> (
      let* record_ptr = compile env state record_ptr in
      match record_ptr.ty with
      | TNum (Base_num _) -> .
      | TRecord _ as recty -> get_field_addr state field recty record_ptr)
  | SetField (field, record_ptr, v) -> (
      let* record_ptr = compile env state record_ptr in
      let* v = compile env state v in
      match record_ptr.ty with
      | TNum (Base_num _) -> .
      | TRecord _ as recty -> set_field state field recty record_ptr v)
  | Cond (cond, ift, iff) ->
      let* cond = compile env state cond in
      let end_of_cond_pos = Llvm.insertion_block (get_builder state) in
      let enclosing_func = Llvm.block_parent end_of_cond_pos in
      (* append a basic block in the current function *)
      let trueblock =
        Llvm.append_block state.llvm_context "iftrue" enclosing_func
      in
      (* position the instruction writer at the end of that new block
           (which is also the beginning since [iftrue] is empty) *)
      Llvm.position_at_end trueblock (get_builder state) ;
      (* codegen into that block *)
      let bt = compile env state ift in
      (* since [codegen_expr] can create new block, we need to get the
           actual block we are in when finishing [codegen_expr gamma iftrue].
           This [trueblock'] is the actual predecessor of the 'continuation'
           block (i.e. the [phi] block). *)
      let trueblock' = Llvm.insertion_block (get_builder state) in
      let falseblock =
        Llvm.append_block state.llvm_context "iffalse" enclosing_func
      in
      Llvm.position_at_end falseblock (get_builder state) ;
      let bf = compile env state iff in
      let falseblock' = Llvm.insertion_block (get_builder state) in
      (* build conditional jump *)
      Llvm.position_at_end end_of_cond_pos (get_builder state) ;
      ignore
        (Llvm.build_cond_br cond.value trueblock falseblock (get_builder state)) ;
      let add_jump_to_join_from ~join ~from =
        Llvm.position_at_end from (get_builder state) ;
        ignore (Llvm.build_br join (get_builder state))
      in
      begin
        match (bt, bf) with
        | (None, None) ->
            (* Both branches raise *)
            None
        | (None, Some _bf) ->
            (* True branch raises *)
            Llvm.position_at_end falseblock' (get_builder state) ;
            let phi_block =
              Llvm.append_block state.llvm_context "ifjoin" enclosing_func
            in
            (* insert jumps from end of the 'false branch' block to the merge node. *)
            add_jump_to_join_from ~join:phi_block ~from:falseblock' ;
            (* Move inserter at end of join block. *)
            Llvm.position_at_end phi_block (get_builder state) ;
            bf
        | (Some _bt, None) ->
            (* False branch raises *)
            Llvm.position_at_end falseblock' (get_builder state) ;
            let phi_block =
              Llvm.append_block state.llvm_context "ifjoin" enclosing_func
            in
            (* insert jumps from end of the 'true branch' block to the merge node. *)
            add_jump_to_join_from ~join:phi_block ~from:trueblock' ;
            (* Move inserter at end of join block. *)
            Llvm.position_at_end phi_block (get_builder state) ;
            bt
        | (Some bt, Some bf) ->
            Llvm.position_at_end falseblock' (get_builder state) ;
            let phi_block =
              Llvm.append_block state.llvm_context "ifjoin" enclosing_func
            in
            Llvm.position_at_end phi_block (get_builder state) ;
            let incoming =
              List.[(bt.value, trueblock'); (bf.value, falseblock')]
            in
            let phi = Llvm.build_phi incoming "phitmp" (get_builder state) in
            add_jump_to_join_from ~join:phi_block ~from:trueblock' ;
            add_jump_to_join_from ~join:phi_block ~from:falseblock' ;
            (* Move inserter at end of join block. *)
            Llvm.position_at_end phi_block (get_builder state) ;
            assert (Types.type_eq bt.ty bf.ty) ;
            with_type bt.ty phi
      end
  | For { init; pred; step; body } ->
      let for_name = random_name () in

      let current_block = Llvm.insertion_block (get_builder state) in

      let for_init =
        create_block_after state current_block (sf "for_%s_init" for_name)
      in
      let for_entry =
        append_to_insertion_block state (sf "for_%s_entry" for_name)
      in
      let for_body =
        append_to_insertion_block state (sf "for_%s_body" for_name)
      in
      let for_exit =
        append_to_insertion_block state (sf "for_%s_exit" for_name)
      in

      (* Add unconditional jump from [current_block] inherited from context to [for_init] *)
      Llvm.position_at_end current_block (get_builder state) ;
      let _ = Llvm.build_br for_init (get_builder state) in
      (* codegen init *)
      Llvm.position_at_end for_init (get_builder state) ;
      let* init = compile env state init in
      let _ = Llvm.build_br for_entry (get_builder state) in

      let last_init_block = Llvm.insertion_block (get_builder state) in

      Llvm.position_at_end for_entry (get_builder state) ;

      let phi_ty = LLVM_type.storage_of_type state.llvm_context Types.i64 in
      let phi =
        Llvm.build_empty_phi
          phi_ty
          (sf "for_%s_phi" for_name)
          (get_builder state)
      in
      Llvm.add_incoming (init.value, last_init_block) phi ;
      let phi_expr = { value = phi; ty = Types.i64 } in

      with_extended_env env phi_expr (fun for_env for_var ->
          let* cond = compile for_env state (pred for_var) in
          let _ =
            Llvm.build_cond_br cond.value for_body for_exit (get_builder state)
          in
          Llvm.position_at_end for_body (get_builder state) ;
          let* _body = compile for_env state (body for_var) in
          let* next = compile for_env state (step for_var) in
          let _ = Llvm.build_br for_entry (get_builder state) in

          Llvm.add_incoming
            (next.value, Llvm.insertion_block (get_builder state))
            phi ;
          Llvm.position_at_end for_exit (get_builder state) ;
          return_unit state)
  | Foldi { init; acc; pred; step; body } ->
      let foldi_name = random_name () in

      let current_block = Llvm.insertion_block (get_builder state) in

      let foldi_init =
        create_block_after state current_block (sf "foldi_%s_init" foldi_name)
      in
      let foldi_entry =
        append_to_insertion_block state (sf "foldi_%s_entry" foldi_name)
      in
      let foldi_body =
        append_to_insertion_block state (sf "foldi_%s_body" foldi_name)
      in
      let foldi_exit =
        append_to_insertion_block state (sf "foldi_%s_exit" foldi_name)
      in

      (* Add unconditional jump from [current_block] inherited from state.llvm_context to [foldi_init] *)
      Llvm.position_at_end current_block (get_builder state) ;
      let _ = Llvm.build_br foldi_init (get_builder state) in
      (* codegen init *)
      Llvm.position_at_end foldi_init (get_builder state) ;
      let* init = compile env state init in
      let* acc = compile env state acc in
      let _ = Llvm.build_br foldi_entry (get_builder state) in

      let last_init_block = Llvm.insertion_block (get_builder state) in

      Llvm.position_at_end foldi_entry (get_builder state) ;

      let counter_ty = LLVM_type.storage_of_type state.llvm_context Types.i64 in
      let phi_counter =
        Llvm.build_empty_phi
          counter_ty
          (sf "foldi_%s_counter_phi" foldi_name)
          (get_builder state)
      in
      Llvm.add_incoming (init.value, last_init_block) phi_counter ;

      let acc_ty = LLVM_type.storage_of_type state.llvm_context acc.ty in
      let phi_acc =
        Llvm.build_empty_phi
          acc_ty
          (sf "foldi_%s_acc_phi" foldi_name)
          (get_builder state)
      in
      Llvm.add_incoming (acc.value, last_init_block) phi_acc ;

      let phi_counter_expr = { value = phi_counter; ty = Types.i64 } in
      let phi_acc_expr = { value = phi_acc; ty = acc.ty } in

      with_extended_env env phi_counter_expr @@ fun env counter_var ->
      with_extended_env env phi_acc_expr @@ fun env acc_var ->
      let* cond = compile env state (pred counter_var acc_var) in
      let _ =
        Llvm.build_cond_br cond.value foldi_body foldi_exit (get_builder state)
      in

      Llvm.position_at_end foldi_body (get_builder state) ;
      let* acc' = compile env state (body counter_var acc_var) in
      let* next = compile env state (step counter_var) in
      let _ = Llvm.build_br foldi_entry (get_builder state) in

      Llvm.add_incoming
        (next.value, Llvm.insertion_block (get_builder state))
        phi_counter ;
      Llvm.add_incoming
        (acc'.value, Llvm.insertion_block (get_builder state))
        phi_acc ;
      Llvm.position_at_end foldi_exit (get_builder state) ;
      Option.some phi_acc_expr
  | While (cond, body) ->
      let while_name = random_name () in

      let current_block = Llvm.insertion_block (get_builder state) in

      let while_cond =
        append_to_insertion_block state (sf "cond_%s_body" while_name)
      in
      let while_body =
        append_to_insertion_block state (sf "while_%s_body" while_name)
      in
      let while_exit =
        append_to_insertion_block state (sf "while_%s_bodexit" while_name)
      in

      (* Insert phi at entry of [while_cond] *)
      Llvm.position_at_end while_cond (get_builder state) ;
      let phi_ty = LLVM_type.storage_of_type state.llvm_context Types.unit in
      let phi =
        Llvm.build_empty_phi
          phi_ty
          (sf "while_%s_phi" while_name)
          (get_builder state)
      in

      (* Add unconditional jump from [current_block] inherited from context to [while_cond] *)
      Llvm.position_at_end current_block (get_builder state) ;
      let _ = Llvm.build_br while_cond (get_builder state) in
      let* u = return_unit state in
      Llvm.add_incoming (u.value, current_block) phi ;

      (* Generate conditional, jump to body or to exit *)
      Llvm.position_at_end while_cond (get_builder state) ;
      let* cond = compile env state cond in
      ignore
        (Llvm.build_cond_br
           cond.value
           while_body
           while_exit
           (get_builder state)) ;

      (* Generate loop body, jump to cond *)
      Llvm.position_at_end while_body (get_builder state) ;
      let* _body = compile env state body in
      ignore (Llvm.build_br while_cond (get_builder state)) ;
      Llvm.add_incoming (u.value, while_body) phi ;

      Llvm.position_at_end while_exit (get_builder state) ;
      return_unit state
  | Switch { e = expr; cases; default } -> (
      let switch_name = random_name () in

      let current_block = Llvm.insertion_block (get_builder state) in

      let non_default_cases = List.length cases in

      let switch_entry =
        create_block_after
          state
          current_block
          (sf "switch_%s_entry" switch_name)
      in

      (* Add unconditional jump from [current_block] inherited from context to [switch_entry] *)
      Llvm.position_at_end current_block (get_builder state) ;
      let _ = Llvm.build_br switch_entry (get_builder state) in

      Llvm.position_at_end switch_entry (get_builder state) ;
      (* Generate code for expr and switch, set default branch *)
      let* expr = compile env state expr in
      let end_of_switch = Llvm.insertion_block (get_builder state) in

      let cases =
        List.mapi
          (fun i (const, case) ->
            let block =
              append_to_insertion_block
                state
                (sf "switch_%s_case_%Ld_%d" switch_name const i)
            in
            (const, case, block))
          cases
      in
      let default_block =
        append_to_insertion_block
          state
          (sf "switch_%s_case_default" switch_name)
      in
      let phi_block =
        append_to_insertion_block state (sf "switch_%s_phi" switch_name)
      in

      Llvm.position_at_end end_of_switch (get_builder state) ;
      let switch =
        Llvm.build_switch
          expr.value
          default_block
          non_default_cases
          (get_builder state)
      in

      (* Build default block *)
      Llvm.position_at_end default_block (get_builder state) ;
      let default = compile env state default in
      let () =
        match default with
        | None -> ()
        | Some _ll -> ignore (Llvm.build_br phi_block (get_builder state))
      in

      let rec loop cases acc =
        match cases with
        | [] -> List.rev acc
        | (const, case, block) :: rest -> (
            Llvm.add_case
              switch
              (Llvm.const_int
                 (LLVM_type.int64_t state.llvm_context)
                 (Int64.to_int const))
              block ;
            Llvm.position_at_end block (get_builder state) ;
            let case = compile env state case in
            match case with
            | None -> loop rest acc
            | Some case ->
                let _ = Llvm.build_br phi_block (get_builder state) in
                loop
                  rest
                  ((case, Llvm.insertion_block (get_builder state)) :: acc))
      in

      let non_default_cases = loop cases [] in
      let all_cases =
        match default with
        | None -> non_default_cases
        | Some default -> (default, default_block) :: non_default_cases
      in
      match all_cases with
      | [] ->
          Llvm.delete_block phi_block ;
          None
      | (first, _) :: _ ->
          Llvm.position_at_end phi_block (get_builder state) ;
          let all_cases =
            List.map (fun (ll, block) -> (ll.value, block)) all_cases
          in
          let result =
            Llvm.build_phi
              all_cases
              (sf "switch_%s_phi_node" switch_name)
              (get_builder state)
          in
          with_type first.ty result)
  | Call (fn, args) -> (
      let* f = compile env state fn in
      match f.ty with
      | TNum (Base_num _) -> .
      | TFn fn ->
          let rec loop : type s ret.
              s fn ->
              (s, ret expr) args ->
              Llvm.llvalue list ->
              ret typed_llvm option =
           fun fn args acc ->
            match fn with
            | Returning retty -> (
                match args with
                | Args_nil ->
                    let llretty =
                      LLVM_type.surface_type state.llvm_context retty
                    in
                    let args = Array.of_list (List.rev acc) in
                    let llfty =
                      Llvm.function_type llretty (Array.map Llvm.type_of args)
                    in
                    let call =
                      Llvm.build_call
                        llfty
                        f.value
                        args
                        "call"
                        (get_builder state)
                    in
                    with_type retty call)
            | Arrow (_, range) -> (
                match args with
                | Args_cons (arg, rest) -> (
                    let arg = compile env state arg in
                    match arg with
                    | None ->
                        failwith "Call: unsound expression in argument position"
                    | Some arg -> loop range rest (arg.value :: acc)))
          in
          loop fn args [])
  | Fundecl fdecl -> Some (fundecl env state fdecl)
  | Fail msg ->
      let fptr =
        match Llvm.lookup_function "failwith" state.llvm_module with
        | None -> failwith "Suplex: failwith not visible from LLVM"
        | Some fptr -> fptr
      in
      let* s = string state ~z:true msg in
      let llretty = LLVM_type.surface_type state.llvm_context TUnit in
      let llfty = Llvm.function_type llretty [| Llvm.type_of s.value |] in
      let _call =
        Llvm.build_call llfty fptr [| s.value |] "call" (get_builder state)
      in
      let _unr = Llvm.build_unreachable (get_builder state) in
      None
  | Malloc typ ->
      let llty = LLVM_type.storage_of_type state.llvm_context typ in
      let llptr = Llvm.build_malloc llty "malloc" (get_builder state) in
      with_type (Types.ptr typ) llptr
  | Malloc_array (typ, len) ->
      let* len = compile env state len in
      let llty = LLVM_type.storage_of_type state.llvm_context typ in
      let llptr =
        Llvm.build_array_malloc
          llty
          len.value
          "array_malloc"
          (get_builder state)
      in
      with_type (Types.arr typ) llptr
  | Free ptr ->
      let* ptr = compile env state ptr in
      let _ = Llvm.build_free ptr.value (get_builder state) in
      return_unit state
  | Free_array arr ->
      let* arr = compile env state arr in
      let _ = Llvm.build_free arr.value (get_builder state) in
      return_unit state
  | Trunc (_n1, n2, v) -> begin
      let target = LLVM_type.of_numerical state.llvm_context n2 in
      let* v = compile env state v in
      let truncated =
        Llvm.build_trunc v.value target "trunc" (get_builder state)
      in
      with_type (TNum n2) truncated
    end
  | ZExt (_n1, n2, v) ->
      let target = LLVM_type.of_numerical state.llvm_context n2 in
      let* v = compile env state v in
      let ext = Llvm.build_zext v.value target "zext" (get_builder state) in
      with_type (TNum n2) ext
  | SExt (_n1, n2, v) ->
      let target = LLVM_type.of_numerical state.llvm_context n2 in
      let* v = compile env state v in
      let ext = Llvm.build_zext v.value target "sext" (get_builder state) in
      with_type (TNum n2) ext
  | ToF32 (n, v) -> (
      let* v = compile env state v in
      let target =
        LLVM_type.of_numerical state.llvm_context (Base_num F32_num)
      in
      match n with
      | I64_num | I32_num | I16_num | I8_num ->
          let fp =
            Llvm.build_sitofp v.value target "si_to_f32" (get_builder state)
          in
          with_type Types.f32 fp
      | F32_num -> Some v
      | F64_num ->
          let fp =
            Llvm.build_fptrunc v.value target "f64_to_f32" (get_builder state)
          in
          with_type Types.f32 fp)
  | ToF64 (n, v) -> (
      let* v = compile env state v in
      let target =
        LLVM_type.of_numerical state.llvm_context (Base_num F32_num)
      in
      match n with
      | I64_num | I32_num | I16_num | I8_num ->
          Llvm.build_sitofp v.value target "si_to_f64" (get_builder state)
          |> with_type Types.f64
      | F64_num -> Some v
      | F32_num ->
          Llvm.build_fpext v.value target "f32_to_f64" (get_builder state)
          |> with_type Types.f64)
  | OfF32 (n, v) -> (
      let target = LLVM_type.of_numerical state.llvm_context (Base_num n) in
      let* v = compile env state v in
      match n with
      | I64_num | I32_num | I16_num | I8_num ->
          Llvm.build_fptosi v.value target "f32_to_si" (get_builder state)
          |> with_type (TNum (Base_num n))
      | F32_num -> Some v
      | F64_num ->
          Llvm.build_fpext v.value target "f32_to_f64" (get_builder state)
          |> with_type Types.f64)
  | OfF64 (n, v) -> (
      let target = LLVM_type.of_numerical state.llvm_context (Base_num n) in
      let* v = compile env state v in
      match n with
      | I64_num | I32_num | I16_num | I8_num ->
          Llvm.build_fptosi v.value target "f64_to_si" (get_builder state)
          |> with_type (TNum (Base_num n))
      | F64_num -> Some v
      | F32_num ->
          Llvm.build_fptrunc v.value target "f64_to_f32" (get_builder state)
          |> with_type (TNum (Base_num n)))
  | VecToF32 (n, v) -> (
      let* v = compile env state v in
      let target_num_ty =
        cast_base_vector_type (numerical_of_type v.ty) F32_num
      in
      let target_ty = TNum target_num_ty in
      let target = LLVM_type.of_numerical state.llvm_context target_num_ty in
      match n with
      | I64_num | I32_num | I16_num | I8_num ->
          let fp =
            Llvm.build_sitofp v.value target "vec_si_to_f32" (get_builder state)
          in
          with_type target_ty fp
      | F32_num -> Some v
      | F64_num ->
          let fp =
            Llvm.build_fptrunc
              v.value
              target
              "vec_f64_to_f32"
              (get_builder state)
          in
          with_type target_ty fp)
  | VecToF64 (n, v) -> (
      let* v = compile env state v in
      let target_num_ty =
        cast_base_vector_type (numerical_of_type v.ty) F64_num
      in
      let target_ty = TNum target_num_ty in
      let target = LLVM_type.of_numerical state.llvm_context target_num_ty in
      match n with
      | I64_num | I32_num | I16_num | I8_num ->
          let fp =
            Llvm.build_sitofp v.value target "vec_si_to_f32" (get_builder state)
          in
          with_type target_ty fp
      | F64_num -> Some v
      | F32_num ->
          let fp =
            Llvm.build_fptrunc
              v.value
              target
              "vec_f64_to_f32"
              (get_builder state)
          in
          with_type target_ty fp)
  | VecOfF32 (n, v) -> (
      let target = LLVM_type.of_numerical state.llvm_context (Base_num n) in
      let* v = compile env state v in
      let target_num_ty = cast_base_vector_type (numerical_of_type v.ty) n in
      let target_ty = TNum target_num_ty in
      match n with
      | I64_num | I32_num | I16_num | I8_num ->
          Llvm.build_fptosi v.value target "f32_to_si" (get_builder state)
          |> with_type target_ty
      | F32_num -> Some v
      | F64_num ->
          Llvm.build_fpext v.value target "f32_to_f64" (get_builder state)
          |> with_type target_ty)
  | VecOfF64 (n, v) -> (
      let target = LLVM_type.of_numerical state.llvm_context (Base_num n) in
      let* v = compile env state v in
      let target_num_ty = cast_base_vector_type (numerical_of_type v.ty) n in
      let target_ty = TNum target_num_ty in
      match n with
      | I64_num | I32_num | I16_num | I8_num ->
          Llvm.build_fptosi v.value target "f64_to_si" (get_builder state)
          |> with_type target_ty
      | F64_num -> Some v
      | F32_num ->
          Llvm.build_fptrunc v.value target "f64_to_f32" (get_builder state)
          |> with_type target_ty)

and get_generic : type a b c.
    environment ->
    llvm_state ->
    (a, c) arr expr ->
    i64 expr ->
    (Llvm.llvalue -> a typ -> b typed_llvm option) ->
    b typed_llvm option =
 fun env state arr i k ->
  let* arr = compile env state arr in
  let* i = compile env state i in
  match (arr.ty, i.ty) with
  | (TNum (Base_num _), _) -> .
  | (TArr_unk elt_ty, TNum (Base_num I64_num))
  | (TArr_cst (elt_ty, _), TNum (Base_num I64_num)) ->
      let llelt_ty = LLVM_type.storage_of_type state.llvm_context elt_ty in
      let addr =
        Llvm.build_gep
          llelt_ty
          arr.value
          [| i.value |]
          "get_gep"
          (get_builder state)
      in
      k addr elt_ty

and alloca : type s. environment -> llvm_state -> s stack -> s * environment =
 fun (type s) env state (frame : s stack) ->
  match frame with
  | End_frame k -> (k, env)
  | Local (SV_unit, rest) ->
      let llty = LLVM_type.storage_of_type state.llvm_context Types.unit in
      let varty = Types.(ptr unit) in
      let llalloca = Llvm.build_alloca llty "loc" (get_builder state) in
      with_extended_env env { value = llalloca; ty = varty } (fun env expr ->
          alloca env state (rest expr))
  | Local (SV_bool, rest) ->
      let llty = LLVM_type.storage_of_type state.llvm_context Types.bool in
      let varty = Types.(ptr bool) in
      let llalloca = Llvm.build_alloca llty "loc" (get_builder state) in
      with_extended_env env { value = llalloca; ty = varty } (fun env expr ->
          alloca env state (rest expr))
  | Local (SV_num n, rest) ->
      let llty = LLVM_type.of_numerical state.llvm_context n in
      let varty = Types.(ptr (TNum n)) in
      let llalloca = Llvm.build_alloca llty "loc" (get_builder state) in
      with_extended_env env { value = llalloca; ty = varty } (fun env expr ->
          alloca env state (rest expr))
  | Local (SV_ptr ty, rest) ->
      let llty = LLVM_type.storage_of_type state.llvm_context (Types.ptr ty) in
      let varty = Types.(ptr (ptr ty)) in
      let llalloca = Llvm.build_alloca llty "loc" (get_builder state) in
      with_extended_env env { value = llalloca; ty = varty } (fun env expr ->
          alloca env state (rest expr))
  | Local (SV_arr (ty, size), rest) -> (
      let lltyp = LLVM_type.storage_of_type state.llvm_context ty in
      let size = compile env state size in
      match size with
      | None -> failwith "Suplex.alloca: size of array is unsound"
      | Some size ->
          let llalloca =
            Llvm.build_array_alloca
              lltyp
              size.value
              "alloca_array"
              (get_builder state)
          in
          with_extended_env
            env
            { value = llalloca; ty = arr ty }
            (fun env expr -> alloca env state (rest expr)))
  | Local (SV_arr_cst (ty, size), rest) ->
      let arr_ty = Types.arr_cst ty size in
      let lltyp = LLVM_type.storage_of_type state.llvm_context arr_ty in
      let llalloca =
        Llvm.build_alloca lltyp "alloca_cst_array" (get_builder state)
      in
      with_extended_env env { value = llalloca; ty = arr_ty } (fun env expr ->
          alloca env state (rest expr))
  | Local (SV_strct r, rest) ->
      let varty = Types.seal r in
      let llty = LLVM_type.storage_of_type state.llvm_context varty in
      let llalloca = Llvm.build_alloca llty "loc" (get_builder state) in
      with_extended_env env { value = llalloca; ty = varty } (fun env expr ->
          alloca env state (rest expr))

and fundecl : type s. environment -> llvm_state -> s fundecl -> s fn typed_llvm
    =
 fun env state { name; sg = signature; body } ->
  save_builder state @@ fun () ->
  let proto = prototype state signature [] in
  let fn = Llvm.declare_function name proto state.llvm_module in
  let _fundecl = with_type (TFn signature) fn in
  let params = Llvm.params fn in
  let bb = Llvm.append_block state.llvm_context "entry" fn in
  Llvm.position_at_end bb (get_builder state) ;
  let (f, env) = alloca env state body in
  let llfn = { value = fn; ty = TFn signature } in
  let res_opt =
    with_extended_env env llfn (fun env fn ->
        apply_args env state signature (Array.to_list params) (f fn))
  in
  let () =
    match res_opt with
    | Error `Bad_arity ->
        failwith "fundecl: LLVM function parameters do not match declared arity"
    | Ok None ->
        (* Function never returns *)
        ()
    | Ok (Some res) -> ignore (Llvm.build_ret res (get_builder state))
  in
  if not (Llvm_analysis.verify_function fn) then (
    (match Llvm_analysis.verify_module state.llvm_module with
    | None -> ()
    | Some reason -> Format.eprintf "Incorrect module.@.%s@." reason) ;
    raise (Invalid_llvm_function (state.llvm_module, fn)))
  else llfn

and apply_args : type s.
    environment ->
    llvm_state ->
    s fn ->
    Llvm.llvalue list ->
    s ->
    (Llvm.llvalue option, [ `Bad_arity ]) Result.t =
 fun env state proto args f ->
  match (proto, args) with
  | (Returning _, []) -> (
      let f = compile env state f in
      match f with None -> Ok None | Some f -> Ok (Some f.value))
  | (Arrow (typ, rest), arg :: args) ->
      with_extended_env env { value = arg; ty = typ } (fun env arg ->
          apply_args env state rest args (f arg))
  | _ -> Error `Bad_arity

and prototype : type s. llvm_state -> s fn -> Llvm.lltype list -> Llvm.lltype =
 fun state proto acc ->
  match proto with
  | Returning ty ->
      let retty = LLVM_type.surface_type state.llvm_context ty in
      Llvm.function_type retty (Array.of_list (List.rev acc))
  | Arrow (ty, rest) ->
      let llty = LLVM_type.surface_type state.llvm_context ty in
      prototype state rest (llty :: acc)
