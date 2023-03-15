module Vec = Vec
module Intf = Intf
module Type_system = Type_system

module Llvm_impl : sig
  include Intf.S with type Exec.cfg = Llvm_executionengine.llcompileroptions

  val set_debug_metadata : string -> unit
end =
  Llvm_impl

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
