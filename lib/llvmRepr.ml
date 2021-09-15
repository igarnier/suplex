open Core
module SMap = Map.Make (String)

type llvm_state =
  { llvm_context : Llvm.llcontext;
    llvm_module : Llvm.llmodule;
    llvm_builder : Llvm.llbuilder;
    externals : Llvm.llvalue SMap.t
  }

module rec State : sig
  type 'a llvm = { llval : Llvm.llvalue; typewit : 'a T.typ }

  type 'a result = { state : llvm_state; result : 'a }

  type 'a t = llvm_state -> 'a result

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val llreturn : Llvm.llvalue -> 'a T.typ -> 'a llvm t

  val context : Llvm.llcontext t

  val lmodule : Llvm.llmodule t

  val builder : Llvm.llbuilder t

  val externals : Llvm.llvalue SMap.t t

  val set_externals : Llvm.llvalue SMap.t -> unit t
end = struct
  type 'a llvm = { llval : Llvm.llvalue; typewit : 'a T.typ }

  type 'a result = { state : llvm_state; result : 'a }

  type 'a t = llvm_state -> 'a result

  let ( >>= ) (m : 'a t) (f : 'a -> 'b t) state =
    let mres = m state in
    f mres.result mres.state

  let return result state = { state; result }

  let llreturn llval typewit state = { state; result = { llval; typewit } }

  let context state = { state; result = state.llvm_context }

  let lmodule state = { state; result = state.llvm_module }

  let builder state = { state; result = state.llvm_builder }

  let externals state = { state; result = state.externals }

  let set_externals externals state =
    let state = { state with externals } in
    { state; result = () }
end

and T : (Type.S with type 'a repr = 'a State.llvm State.t) = Type.Inst (struct
  type nonrec 'a repr = 'a State.llvm State.t
end)

type 'a repr = 'a State.llvm State.t

type 'a cont = { cont : 'b. ('a -> 'b repr) -> 'b repr }

(* -------------------------------------------------------------------------- *)
(* Handling types *)

(* Basic Llvm types. *)
let _void ctxt = Llvm.void_type ctxt

let int64_t ctxt = Llvm.i64_type ctxt

let int32_t ctxt = Llvm.i32_type ctxt

let int16_t ctxt = Llvm.i16_type ctxt

let int8_t ctxt = Llvm.i8_type ctxt

let size_t ctxt = Llvm.i64_type ctxt

let bool_t ctxt = Llvm.i1_type ctxt

let float_t ctxt = Llvm.float_type ctxt

let double_t ctxt = Llvm.double_type ctxt

(* Convert type to Llvm repr *)
let rec type_to_llvm : type a. a T.typ -> Llvm.lltype State.t =
  fun (type a) (typ : a T.typ) : Llvm.lltype State.t ->
   let open State in
   let open T in
   context >>= fun ctxt ->
   match typ with
   | TNum typ -> numerical_type_to_llvm typ
   | TUnit -> return @@ int8_t ctxt
   | TBool -> return @@ bool_t ctxt
   | TPtr typ ->
       type_to_llvm typ >>= fun lltyp -> return @@ Llvm.pointer_type lltyp
   | TVec typ ->
       type_to_llvm typ >>= fun lltyp -> return @@ Llvm.pointer_type lltyp
   | TFun { arg; ret } ->
       type_to_llvm ret >>= fun llrettyp ->
       type_to_llvm arg >>= fun llargtyp ->
       return @@ Llvm.function_type llrettyp [| llargtyp |]
   | TTpl tuple ->
       tuple_type_to_llvm_struct tuple >>= fun struct_type ->
       return (Llvm.pointer_type struct_type)

and numerical_type_to_llvm : type a. a T.numerical -> Llvm.lltype State.t =
  fun (type a) (typ : a T.numerical) : Llvm.lltype State.t ->
   let open State in
   let open T in
   context >>= fun ctxt ->
   match typ with
   (* Unit should logically be [void_type context], no? TODO *)
   | Int64 -> return @@ int64_t ctxt
   | Int32 -> return @@ int32_t ctxt
   | Int16 -> return @@ int16_t ctxt
   | Int8 -> return @@ int8_t ctxt
   | Float -> return @@ float_t ctxt
   | Double -> return @@ double_t ctxt

and tuple_type_to_llvm_struct (tuple : T.ex_typ list) : Llvm.lltype State.t =
  let open State in
  let rec loop (tuple : T.ex_typ list) acc =
    match tuple with
    | [] -> return (List.rev acc)
    | T.Ex_typ field_type :: tl ->
        type_to_llvm field_type >>= fun typ -> loop tl (typ :: acc)
  in
  context >>= fun ctxt ->
  loop tuple [] >>= fun res ->
  return @@ Llvm.struct_type ctxt (Array.of_list res)

(* -------------------------------------------------------------------------- *)
(* Access to externals *)

module Externals = struct
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

  type t = { name : name; typ : T.ex_funtype }

  let name = function
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
    let open T in
    [ { name = Print_int64; typ = Ex_funtype { arg = TNum Int64; ret = TUnit } };
      { name = Print_int32; typ = Ex_funtype { arg = TNum Int32; ret = TUnit } };
      { name = Print_int16; typ = Ex_funtype { arg = TNum Int16; ret = TUnit } };
      { name = Print_int8; typ = Ex_funtype { arg = TNum Int8; ret = TUnit } };
      { name = Print_float; typ = Ex_funtype { arg = TNum Float; ret = TUnit } };
      { name = Print_double;
        typ = Ex_funtype { arg = TNum Double; ret = TUnit }
      };
      { name = Print_bool; typ = Ex_funtype { arg = TBool; ret = TUnit } };
      { name = Print_unit; typ = Ex_funtype { arg = TUnit; ret = TUnit } };
      { name = Instralloc;
        typ = Ex_funtype { arg = TNum Int64; ret = TPtr (TNum Int8) }
      } ]
end

let register_external (ext : Externals.t) : unit State.t =
  let open State in
  match ext.Externals.typ with
  | T.Ex_funtype typ ->
      type_to_llvm (TFun typ) >>= fun ft ->
      lmodule >>= fun llmodule ->
      externals >>= fun map ->
      let name = Externals.name ext.name in
      let fv = Llvm.declare_function name ft llmodule in
      set_externals (SMap.add_exn map ~key:name ~data:fv)

let register_all_externals : unit State.t =
  let open State in
  let rec loop l =
    match l with
    | [] -> return ()
    | ext :: tl -> register_external ext >>= fun () -> loop tl
  in
  loop Externals.all

let get_external ~name : Llvm.llvalue State.t =
 fun state ->
  let name = Externals.name name in
  match SMap.find state.externals name with
  | None ->
      let msg = Printf.sprintf "Compile.get_external: %s not found" name in
      failwith msg
  | Some result -> State.{ state; result }

let type_is_function (type t) (typ : t T.typ) : bool =
  match typ with
  | T.TFun _ -> true
  | T.TBool -> false
  | T.TUnit -> false
  | T.TNum _ -> false
  | T.TPtr _ -> false
  | T.TVec _ -> false
  | T.TTpl _ -> false

let build_malloc_fptr lltype count =
  let open State in
  builder >>= fun bld ->
  context >>= fun ctxt ->
  lmodule >>= fun llmodule ->
  match Llvm.lookup_function "instralloc" llmodule with
  | Some malloc ->
      let lltype = Llvm.pointer_type lltype in
      let sizeof = Llvm.size_of lltype in
      let bytesize = Llvm.build_mul count sizeof "multmp" bld in
      let downcast = Llvm.build_trunc bytesize (size_t ctxt) "casttmp" bld in
      let ptr = Llvm.build_call malloc [| downcast |] "calltmp" bld in
      let llvalue =
        Llvm.build_bitcast ptr (Llvm.pointer_type lltype) "alloc_ptr" bld
      in
      return llvalue
  | None -> failwith "Codegen.build_malloc_fptr: instralloc not declared"

let build_malloc_immediate lltype count =
  let open State in
  builder >>= fun bld ->
  context >>= fun ctxt ->
  lmodule >>= fun llmodule ->
  match Llvm.lookup_function "instralloc" llmodule with
  | Some malloc ->
      let sizeof =
        Llvm.const_intcast (Llvm.size_of lltype) (size_t ctxt) ~is_signed:false
      in
      let bytesize = Llvm.build_mul count sizeof "multmp" bld in
      let downcast = Llvm.build_trunc bytesize (size_t ctxt) "casttmp" bld in
      let ptr = Llvm.build_call malloc [| downcast |] "calltmp" bld in
      let llvalue =
        Llvm.build_bitcast ptr (Llvm.pointer_type lltype) "alloc_ptr" bld
      in
      return llvalue
  | None -> failwith "Codegen.build_malloc: instralloc not declared"

let build_malloc (type a) (count : Llvm.llvalue) (typ : a T.typ) :
    Llvm.llvalue State.t =
  let open State in
  type_to_llvm typ >>= fun lltype ->
  match typ with
  | T.TFun _ -> build_malloc_fptr lltype count
  | T.TUnit -> build_malloc_immediate lltype count
  | T.TNum _ -> build_malloc_immediate lltype count
  | T.TTpl fields ->
      tuple_type_to_llvm_struct fields >>= fun llstructtype ->
      build_malloc_immediate llstructtype count
  | _ -> assert false

(* -------------------------------------------------------------------------- *)
(* Implement the Lang.S interface *)

let unit =
  let open State in
  context >>= fun ctx -> llreturn (Llvm.const_int (int8_t ctx) 0) T.TUnit

let int64 (i : int) =
  let open State in
  context >>= fun ctx ->
  llreturn (Llvm.const_int (int64_t ctx) i) T.(TNum Int64)

let int32 (i : int) =
  let open State in
  context >>= fun ctx ->
  llreturn (Llvm.const_int (int32_t ctx) i) T.(TNum Int32)

let int16 (i : int) =
  let open State in
  context >>= fun ctx ->
  llreturn (Llvm.const_int (int16_t ctx) i) T.(TNum Int16)

let int8 (i : int) =
  let open State in
  context >>= fun ctx -> llreturn (Llvm.const_int (int8_t ctx) i) T.(TNum Int8)

let float (f : float) =
  let open State in
  context >>= fun ctx ->
  llreturn (Llvm.const_float (float_t ctx) f) T.(TNum Float)

let double (f : float) =
  let open State in
  context >>= fun ctx ->
  llreturn (Llvm.const_float (double_t ctx) f) T.(TNum Double)

let tt =
  let open State in
  context >>= fun ctx -> llreturn (Llvm.const_int (bool_t ctx) 1) T.TBool

let ff =
  let open State in
  context >>= fun ctx -> llreturn (Llvm.const_int (bool_t ctx) 0) T.TBool

let add (type t) (numtyp : t T.numerical) (lhs : t repr) (rhs : t repr) =
  let open State in
  lhs >>= fun lhs_v ->
  rhs >>= fun rhs_v ->
  builder >>= fun bld ->
  let llval =
    match T.numerical_kind numtyp with
    | `int -> Llvm.build_add lhs_v.llval rhs_v.llval "int_add_tmp" bld
    | `fp -> Llvm.build_fadd lhs_v.llval rhs_v.llval "float_add_tmp" bld
  in
  llreturn llval T.(TNum numtyp)

let sub (type t) (numtyp : t T.numerical) (lhs : t repr) (rhs : t repr) =
  let open State in
  lhs >>= fun lhs_v ->
  rhs >>= fun rhs_v ->
  builder >>= fun bld ->
  let llval =
    match T.numerical_kind numtyp with
    | `int -> Llvm.build_sub lhs_v.llval rhs_v.llval "int_sub_tmp" bld
    | `fp -> Llvm.build_fsub lhs_v.llval rhs_v.llval "float_sub_tmp" bld
  in
  llreturn llval T.(TNum numtyp)

let mul (type t) (numtyp : t T.numerical) (lhs : t repr) (rhs : t repr) =
  let open State in
  lhs >>= fun lhs_v ->
  rhs >>= fun rhs_v ->
  builder >>= fun bld ->
  let llval =
    match T.numerical_kind numtyp with
    | `int -> Llvm.build_mul lhs_v.llval rhs_v.llval "int_mul_tmp" bld
    | `fp -> Llvm.build_fmul lhs_v.llval rhs_v.llval "float_mul_tmp" bld
  in
  llreturn llval T.(TNum numtyp)

let div (type t) (numtyp : t T.numerical) (lhs : t repr) (rhs : t repr) =
  let open State in
  lhs >>= fun lhs_v ->
  rhs >>= fun rhs_v ->
  builder >>= fun bld ->
  let llval =
    match T.numerical_kind numtyp with
    | `int -> Llvm.build_sdiv lhs_v.llval rhs_v.llval "int_sdiv_tmp" bld
    | `fp -> Llvm.build_fdiv lhs_v.llval rhs_v.llval "float_div_tmp" bld
  in
  llreturn llval T.(TNum numtyp)

let ( && ) lhs rhs =
  let open State in
  lhs >>= fun lhs_v ->
  rhs >>= fun rhs_v ->
  builder >>= fun bld ->
  let llval = Llvm.build_and lhs_v.llval rhs_v.llval "bool_and_tmp" bld in
  llreturn llval T.TBool

let ( || ) lhs rhs =
  let open State in
  lhs >>= fun lhs_v ->
  rhs >>= fun rhs_v ->
  builder >>= fun bld ->
  let llval = Llvm.build_or lhs_v.llval rhs_v.llval "bool_and_tmp" bld in
  llreturn llval T.TBool

let lt (type t) (numtyp : t T.numerical) (lhs : t repr) (rhs : t repr) =
  let open State in
  lhs >>= fun lhs_v ->
  rhs >>= fun rhs_v ->
  builder >>= fun bld ->
  let llval =
    match T.numerical_kind numtyp with
    | `int ->
        Llvm.build_icmp Llvm.Icmp.Slt lhs_v.llval rhs_v.llval "int_lt_tmp" bld
    | `fp ->
        Llvm.build_fcmp Llvm.Fcmp.Olt lhs_v.llval rhs_v.llval "float_lt_tmp" bld
  in
  llreturn llval T.TBool

let le (type t) (numtyp : t T.numerical) (lhs : t repr) (rhs : t repr) =
  let open State in
  lhs >>= fun lhs_v ->
  rhs >>= fun rhs_v ->
  builder >>= fun bld ->
  let llval =
    match T.numerical_kind numtyp with
    | `int ->
        Llvm.build_icmp Llvm.Icmp.Sle lhs_v.llval rhs_v.llval "int_le_tmp" bld
    | `fp ->
        Llvm.build_fcmp Llvm.Fcmp.Ole lhs_v.llval rhs_v.llval "float_le_tmp" bld
  in
  llreturn llval T.TBool

let eq (type t) (numtyp : t T.numerical) (lhs : t repr) (rhs : t repr) =
  let open State in
  lhs >>= fun lhs_v ->
  rhs >>= fun rhs_v ->
  builder >>= fun bld ->
  let llval =
    match T.numerical_kind numtyp with
    | `int ->
        Llvm.build_icmp Llvm.Icmp.Eq lhs_v.llval rhs_v.llval "int_eq_tmp" bld
    | `fp ->
        Llvm.build_fcmp Llvm.Fcmp.Oeq lhs_v.llval rhs_v.llval "float_eq_tmp" bld
  in
  llreturn llval T.TBool

let print (type t) (arg : t repr) =
  let open State in
  arg >>= fun arg_v ->
  let func =
    match arg_v.typewit with
    | T.TNum T.Int64 -> get_external ~name:Externals.Print_int64
    | T.TNum T.Int32 -> get_external ~name:Externals.Print_int32
    | T.TNum T.Int16 -> get_external ~name:Externals.Print_int16
    | T.TNum T.Int8 -> get_external ~name:Externals.Print_int8
    | T.TNum T.Float -> get_external ~name:Externals.Print_float
    | T.TNum T.Double -> get_external ~name:Externals.Print_double
    | T.TBool -> get_external ~name:Externals.Print_bool
    | T.TUnit -> get_external ~name:Externals.Print_unit
    | _ ->
        failwith
          "LlvmRepr.print: cannot print pointer, vector or functional values"
  in
  func >>= fun func_v ->
  builder >>= fun bld ->
  let llvalue = Llvm.build_call func_v [| arg_v.llval |] "calltmp" bld in
  llreturn llvalue T.TUnit

type ex_result = Ex_result : 'a State.llvm -> ex_result

(* codegen components and wrap existentially their types *)
let rec evaluate_tuple l acc =
  let open State in
  match l with
  | [] -> return (List.rev acc)
  | T.Ex_repr repr :: tail ->
      repr >>= fun repr_v -> evaluate_tuple tail (Ex_result repr_v :: acc)

let tuple (tuple : T.ex_repr list) =
  let open State in
  evaluate_tuple tuple [] >>= fun components ->
  let types =
    List.map components ~f:(function Ex_result repr_v ->
        T.(Ex_typ repr_v.typewit))
  in
  let typ = T.TTpl types in
  int64 1 >>= fun count ->
  build_malloc count.llval typ >>= fun tuple_ptr ->
  builder >>= fun bld ->
  let rec initialize l index =
    match l with
    | [] -> ()
    | Ex_result repr_v :: tail ->
        let instr_name = "fieldaddr_" ^ string_of_int index in
        let field_ptr = Llvm.build_struct_gep tuple_ptr index instr_name bld in
        let _ = Llvm.build_store repr_v.llval field_ptr bld in
        initialize tail (index + 1)
  in
  initialize components 0 ;
  llreturn tuple_ptr typ

let proj (type a) tuple ({ index; desired_type } : a T.access) =
  let open State in
  tuple >>= fun tuple_v ->
  builder >>= fun bld ->
  match tuple_v.typewit with
  | T.TTpl typ -> (
      let indexed_typ = List.nth typ index in
      match indexed_typ with
      | None -> failwith "LlvmRepr.proj: index out of bounds"
      | Some (T.Ex_typ actual_type) -> (
          match T.type_eq desired_type actual_type with
          | None ->
              let desired_s = T.show_typ desired_type in
              let actual_s = T.show_typ actual_type in
              let msg =
                Printf.sprintf
                  "LlvmRepr.proj: type mismatch, expected: %s vs actual: %s"
                  desired_s
                  actual_s
              in
              failwith msg
          | Some _ ->
              let elt_ptr =
                Llvm.build_struct_gep
                  tuple_v.llval
                  index
                  ("fieldaddr_" ^ string_of_int index)
                  bld
              in
              let llvalue = Llvm.build_load elt_ptr "proj_load" bld in
              llreturn llvalue desired_type))

let ( >|= ) (type t u) (m : t repr) (f : t repr -> u repr) state =
  let m_v = m state in
  f (fun _ -> m_v) state

let cond (type t) (cond : T.bool_t repr) (bt : t repr) (bf : t repr) =
  let open State in
  builder >>= fun bld ->
  context >>= fun ctxt ->
  cond >>= fun cond_v ->
  let end_of_cond_pos = Llvm.insertion_block bld in
  let enclosing_func = Llvm.block_parent end_of_cond_pos in
  (* append a basic block in the current function *)
  let trueblock = Llvm.append_block ctxt "iftrue" enclosing_func in
  (* position the instruction writer at the end of that new block
         (which is also the beginning since [iftrue] is empty) *)
  Llvm.position_at_end trueblock bld ;
  (* codegen into that block *)
  bt >>= fun bt_v ->
  (* since [codegen_expr] can create new block, we need to get the
         actual block we are in when finishing [codegen_expr gamma iftrue].
         This [trueblock'] is the actual predecessor of the 'continuation'
         block (i.e. the [phi] block). *)
  let trueblock' = Llvm.insertion_block bld in
  let falseblock = Llvm.append_block ctxt "iffalse" enclosing_func in
  Llvm.position_at_end falseblock bld ;
  bf >>= fun bf_v ->
  let falseblock' = Llvm.insertion_block bld in
  (* emit code for the 'join' aka 'continuation' aka 'phi' block *)
  let joinblock = Llvm.append_block ctxt "ifjoin" enclosing_func in
  Llvm.position_at_end joinblock bld ;
  let incoming = [(bt_v.llval, trueblock'); (bf_v.llval, falseblock')] in
  let phi = Llvm.build_phi incoming "phitmp" bld in
  (* move the instruction builder back at he the end of [end_of_cond_pos],
         emit appropriate jump. *)
  Llvm.position_at_end end_of_cond_pos bld ;
  ignore (Llvm.build_cond_br cond_v.llval trueblock falseblock bld) ;
  (* insert jumps from end of the 'true branch' block to the merge node. *)
  Llvm.position_at_end trueblock' bld ;
  ignore (Llvm.build_br joinblock bld) ;
  (* insert jumps from end of the 'true branch' block to the merge node. *)
  Llvm.position_at_end falseblock' bld ;
  ignore (Llvm.build_br joinblock bld) ;
  (* Move inserter at end of join block. *)
  Llvm.position_at_end joinblock bld ;
  llreturn phi bf_v.typewit

(* let decl (type t) (numtyp : t T.numerical) =
 *   { cont = (fun (type b) (k : t T.ptr_t repr -> b repr) -> assert false) } *)

let fresh_state () =
  let llvm_context = Llvm.global_context () in
  let llvm_module = Llvm.create_module llvm_context "jit" in
  let llvm_builder = Llvm.builder llvm_context in
  let externals = SMap.empty in
  register_all_externals { llvm_context; llvm_module; llvm_builder; externals }
