(* open Type_system *)

(* (\** The module type of numerical types. *\) *)
(* module type Numerical = sig *)
(*   type !'a typ *)

(*   type !'a m *)

(*   (\** [t] is the numerical type. *\) *)
(*   type t *)

(*   (\** [v] is the type of constant values. *\) *)
(*   type v *)

(*   (\** Witness that [t] is a numerical type. *\) *)
(*   val n : t numerical *)

(*   (\** [v c] is a constant equal to [c]. *\) *)
(*   val v : v -> t m *)

(*   val add : t m -> t m -> t m *)

(*   val sub : t m -> t m -> t m *)

(*   val mul : t m -> t m -> t m *)

(*   val div : t m -> t m -> t m *)

(*   val neg : t m -> t m *)

(*   val lt : t m -> t m -> bool m *)

(*   val le : t m -> t m -> bool m *)

(*   val eq : t m -> t m -> bool m *)

(*   val zero : t m *)

(*   val one : t m *)
(* end *)

(* (\** The module type describing casting between numerical types. *\) *)
(* module type Cast = sig *)
(*   type 'a m *)

(*   (\** [trunc n1 n2] constructs a truncating cast from [n1] to [n2]. Returns *)
(*       [None] if a truncation doesn't exist, e.g. if [n1] is smaller than [n2]. *)
(*   *\) *)
(*   val trunc : 'a numerical -> 'b numerical -> 'a m -> 'b m option *)

(*   (\** [zext n1 n2] constructs a zero-extending cast from [n1] to [n2]. *\) *)
(*   val zext : 'a numerical -> 'b numerical -> 'a m -> 'b m option *)

(*   (\** [zext n1 n2] constructs a sign-extending cast from [n1] to [n2]. *\) *)
(*   val sext : 'a numerical -> 'b numerical -> 'a m -> 'b m option *)

(*   (\** [f32 n] constructs a cast from [n] to [f32]. *\) *)
(*   val f32 : 'a numerical -> 'a m -> f32 m *)

(*   (\** [f64 n] constructs a cast from [n] to [f64]. *\) *)
(*   val f64 : 'a numerical -> 'a m -> f64 m *)

(*   (\** [of_f32 n] constructs a cast from [f32] to [n]. *\) *)
(*   val of_f32 : 'a numerical -> f32 m -> 'a m *)

(*   (\** [of_f64 n] constructs a cast from [f64] to [n]. *\) *)
(*   val of_f64 : 'a numerical -> f64 m -> 'a m *)
(* end *)

(* module type Stack_frame = sig *)
(*   type (_, _, _, _) record *)

(*   type !'a typ *)

(*   type !'a m *)

(*   type !'a ptr *)

(*   type (!'a, 'c) arr *)

(*   (\** [stack_var] describes items on the function stack. *\) *)
(*   type 'a stack_var = *)
(*     | SV_unit : unit ptr stack_var *)
(*     | SV_bool : bool ptr stack_var *)
(*     | SV_num : 'a numerical -> 'a ptr stack_var *)
(*     | SV_ptr : 'a typ -> 'a ptr ptr stack_var *)
(*     | SV_arr : 'a typ * i64 m -> ('a, [ `unk ]) arr stack_var *)
(*     | SV_arr_cst : 'a typ * int64 -> ('a, [ `cst ]) arr stack_var *)
(*     | SV_strct : (_, 'd vec, 'd vec, 't) record -> 't ptr stack_var *)

(*   (\** [t] is the type of descriptions of function stacks. *\) *)
(*   type (_, _) t = *)
(*     | Empty : ('b, 'b) t *)
(*     | Cons : 'a stack_var * ('c, 'b) t -> ('a m -> 'c, 'b) t *)

(*   (\** [empty] is the empty stack. *\) *)
(*   val empty : ('b, 'b) t *)

(*   (\** [elt @+ rest] describes a stack with [elt] on top of [rest]. *\) *)
(*   val ( @+ ) : 'a stack_var -> ('c, 'b) t -> ('a m -> 'c, 'b) t *)

(*   (\** [unit] describes a stack element of type [unit]. *\) *)
(*   val unit : unit ptr stack_var *)

(*   (\** [bool] describes a stack element of type [bool]. *\) *)
(*   val bool : bool ptr stack_var *)

(*   (\** [num n] describes a stack element of type [n]. *\) *)
(*   val num : 'a numerical -> 'a ptr stack_var *)

(*   (\** [ptr ty] describes a stack element of type [ptr ty]. *\) *)
(*   val ptr : 'a typ -> 'a ptr ptr stack_var *)

(*   (\** [arr ty len] describes a stack element storing an array of [ty] with *)
(*       length [len]. *\) *)
(*   val arr : 'a typ -> i64 m -> ('a, [ `unk ]) arr stack_var *)

(*   (\** [arr_cst ty len] describes a stack element storing an array of [ty] with *)
(*       statically known length [len]. *\) *)
(*   val arr_cst : 'a typ -> int64 -> ('a, [ `cst ]) arr stack_var *)

(*   (\** [strct r] describes a stack element storing a struct of type [r]. *\) *)
(*   val strct : (_, 'd vec, 'd vec, 't) record -> 't ptr stack_var *)
(* end *)

(* (\** The module type of code generation monads. *\) *)
(* module type Monad = sig *)
(*   (\** ['a t] is the type of computations happening in the code generation monad. *)
(*       Such a computation might generate code as a side effect. *\) *)
(*   type +!'a t *)

(*   (\** [cfg] is the type of parameters for running an effectful computation *)
(*       generating code. Typically, this will correspond to parameters of the code *)
(*       generator. *\) *)
(*   type cfg *)

(*   (\** [state] is the type of internal state of the code generation monad. *\) *)
(*   type state *)

(*   (\** The default configuration. *\) *)
(*   val default_cfg : unit -> cfg *)

(*   val return : 'a -> 'a t *)

(*   val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t *)

(*   val run : cfg -> 'a t -> state * 'a *)
(* end *)

(* (\** [S] is the main module type provided by {b suplex}. It exposes a low-level, *)
(*     C-like language. Users describe programs in this language and {b suplex} *)
(*     provides facilities to run the code from OCaml. *\) *)
(* module type S = sig *)
(*   type +!'a k *)

(*   type (!'a, 'typ) expr *)

(*   type !'a ptr *)

(*   type (!'a, 'c) arr *)

(*   (\** [K] is the codegen monad. *\) *)
(*   module K : Monad with type 'a t = 'a k *)

(*   (\** [Types] describes the type system of the low-level language. *\) *)
(*   module Types : *)
(*     Type_system.S *)
(*       with type ('a, 'typ) typed_term = ('a, 'typ) expr k *)
(*        and type 'a ptr = 'a ptr *)
(*        and type ('a, 'c) arr = ('a, 'c) arr *)

(*   type 'a m = 'a Types.m *)

(*   type 'a typ = 'a Types.typ *)

(*   (\** The module type of numerical types (see {!Numerical}) *\) *)
(*   module type Numerical = *)
(*     Numerical with type 'a typ := 'a typ and type 'a m = 'a m *)

(*   (\** The module type of casting operations (see {!module-type-Cast}) *\) *)
(*   module type Cast = Cast with type 'a m := 'a m *)

(*   (\** The [Stack] module is used to construct stack descriptors for function *)
(*       declarations. *\) *)
(*   module Stack : *)
(*     Stack_frame *)
(*       with type ('a, 'b, 'c, 'd) record := ('a, 'b, 'c, 'd) Types.record *)
(*        and type 'a typ := 'a typ *)
(*        and type 'a m := 'a m *)
(*        and type 'a ptr := 'a ptr *)
(*        and type ('a, 'c) arr := ('a, 'c) arr *)

(*   (\** ['a fundecl] is the type of functions with prototype ['a]. *\) *)
(*   type 'a fundecl *)

(*   (\** [fundecl_name n] returns the name of a function declaration. *\) *)
(*   val fundecl_name : 'a fundecl -> string *)

(*   (\** [register_external ~name proto] returns a function with name [name] and *)
(*       prototype [proto], assuming that a [C] function with the corresponding *)
(*       name and prototype is linked with the currently running program. *)

(*       @raise Failure if the function is not available. *\) *)
(*   val register_external : name:string -> 's Types.fn -> 's fundecl k *)

(*   (\** {2 Language constructors} *)

(*       {b suplex} is expression-based and strongly typed, but not memory safe. *)
(*       The type ['a m] denotes expression that will evaluate at codegen time to *)
(*       values of type ['a]. *\) *)

(*   (\** [unit] is the unit value. *\) *)
(*   val unit : unit m *)

(*   (\** [tt] is the [true] boolean constant. *\) *)
(*   val tt : bool m *)

(*   (\** [ff] is the [false] boolean constant. *\) *)
(*   val ff : bool m *)

(*   (\** [string ?z str] constructs a constant string with contents [str]. If [z] *)
(*       is set to false, the resulting string is not zero-terminated. *\) *)
(*   val string : ?z:bool -> string -> i8 ptr m *)

(*   (\** Boolean conjunction. *\) *)
(*   val ( && ) : bool m -> bool m -> bool m *)

(*   (\** Boolean disjunction. *\) *)
(*   val ( || ) : bool m -> bool m -> bool m *)

(*   (\** [I64] is the numerical type of signed 64 bits integers. *\) *)
(*   module I64 : Numerical with type t = i64 and type v = int64 *)

(*   (\** [I32] is the numerical type of signed 32 bits integers. *\) *)
(*   module I32 : Numerical with type t = i32 and type v = int32 *)

(*   (\** [I16] is the numerical type of signed 16 bits integers. *\) *)
(*   module I16 : Numerical with type t = i16 and type v = int *)

(*   (\** [I8] is the numerical type of signed 8 bits integers. *\) *)
(*   module I8 : Numerical with type t = i8 and type v = char *)

(*   (\** [F32] is the numerical type of single-precision floating-point values. *\) *)
(*   module F32 : Numerical with type t = f32 and type v = float *)

(*   (\** [F64] is the numerical type of double-precision floating-point values. *\) *)
(*   module F64 : Numerical with type t = f64 and type v = float *)

(*   (\** [projs r] constructs the projection functions out of a record description *)
(*       [r]. *\) *)
(*   val projs : ('elim, 'a vec, 'a vec, 'u) Types.record -> 'elim *)

(*   (\** [strct.%{field}] evaluates the projection [field] on the structure *)
(*       [strct]. *)

(*       Note that the structure is accessed through a pointer. Accessing a badly *)
(*       or null pointer is undefined behaviour. *\) *)
(*   val ( .%{} ) : 'a ptr m -> ('b, 'a) Types.proj -> 'b m *)

(*   (\** [strct.%{field}] evaluates the projection [field] on the structure [strct] *)
(*       and returns the {b address} of the data for that [field]. *)

(*       Note that the structure is accessed through a pointer. Accessing a badly *)
(*       or null pointer is undefined behaviour. *\) *)
(*   val ( .&{} ) : 'a ptr m -> ('b, 'a) Types.proj -> 'b ptr m *)

(*   (\** [strct.%{field} <- v] assigns the value [v] to [field] in [strct]. *)

(*       Note that the structure is accessed through a pointer. Accessing a badly *)
(*       or null pointer is undefined behaviour. *\) *)
(*   val ( .%{}<- ) : 'a ptr m -> ('b, 'a) Types.proj -> 'b m -> unit m *)

(*   (\** [seq m @@ fun () -> rest] corresponds to evaluating [m] sequentially then *)
(*       evaluating [rest]. This is equivalent to [let*! _ = m in rest]. *\) *)
(*   val seq : unit m -> (unit -> 'a m) -> 'a m *)

(*   (\** [let*! x = m in rest] performs code generation for [m], binds the result *)
(*       to [x] and perform code generation for [m]. *)

(*       Note that this is not equivalent to simply substituting [x] for [m] in *)
(*       [rest]. *\) *)
(*   val ( let*! ) : 'a m -> ('a m -> 'b k) -> 'b k *)

(*   (\** [store ptr v] stores [v] at the address pointed to by [ptr]. *\) *)
(*   val store : 'a ptr m -> 'a m -> unit m *)

(*   (\** [load ptr] loads the value pointed by [ptr]. *\) *)
(*   val load : 'a ptr m -> 'a m *)

(*   (\** [null_ptr ty] is the null pointer of type [ty]. *\) *)
(*   val null_ptr : 'a typ -> 'a ptr m *)

(*   (\** [is_null ptr] evaluates to [true] if [ptr] is null. *\) *)
(*   val is_null : 'a ptr m -> bool m *)

(*   (\** [ptr_eq ptr1 ptr2] performs a pointer equality test.*\) *)
(*   val ptr_eq : 'a ptr m -> 'a ptr m -> bool m *)

(*   (\** [set arr i v] sets the value [v] at index [i] in [arr]. No bound checking *)
(*       is performed. *\) *)
(*   val set : ('a, 'c) arr m -> i64 m -> 'a m -> unit m *)

(*   (\** [setaddr arr i v] sets the address of the first element of [v] at index *)
(*       [i] in [arr]. No bound checking is performed. *\) *)
(*   val setaddr : *)
(*     (('a, 'd) arr ptr, 'c) arr m -> i64 m -> ('a, 'd) arr m -> unit m *)

(*   (\** [get arr i] loads the element at index [i] in [arr]. *\) *)
(*   val get : ('a, 'c) arr m -> i64 m -> 'a m *)

(*   (\** [getaddr arr i] returns the address of the element at index [i] in [arr]. *)
(*   *\) *)
(*   val getaddr : ('a, 'c) arr m -> i64 m -> 'a ptr m *)

(*   (\** Alias to [get]. *\) *)
(*   val ( .%[] ) : ('a, 'c) arr m -> i64 m -> 'a m *)

(*   (\** Alias to [getaddr]. *\) *)
(*   val ( .&[] ) : ('a, 'c) arr m -> i64 m -> 'a ptr m *)

(*   (\** Alias to [set]. *\) *)
(*   val ( .%[]<- ) : ('a, 'c) arr m -> i64 m -> 'a m -> unit m *)

(*   (\** Alias to [setaddr]. *\) *)
(*   val ( .&[]<- ) : *)
(*     (('a, 'd) arr ptr, 'c) arr m -> i64 m -> ('a, 'd) arr m -> unit m *)

(*   (\** [for_ ~init ~pred ~step body] generates a for loop. This is implements the *)
(*       C-like form *)
(*       {[ *)
(*         for (init; pred; step) body *)
(*       ]} *\) *)
(*   val for_ : *)
(*     init:i64 m -> *)
(*     pred:(i64 m -> bool m) -> *)
(*     step:(i64 m -> i64 m) -> *)
(*     (i64 m -> unit m) -> *)
(*     unit m *)

(*   (\** [foldi_ ~init ~acc ~pred ~step body] generates a fold over the sequence of *)
(*       integers defined by [init], [pred] and [step]. *\) *)
(*   val foldi : *)
(*     init:i64 m -> *)
(*     acc:'a m -> *)
(*     pred:(i64 m -> 'a m -> bool m) -> *)
(*     step:(i64 m -> i64 m) -> *)
(*     (i64 m -> 'a m -> 'a m) -> *)
(*     'a m *)

(*   val while_ : (unit -> bool m) -> (unit -> unit m) -> unit m *)

(*   (\** [cond c body] constructs a conditional expression. *\) *)
(*   val cond : bool m -> (bool -> 'a m) -> 'a m *)

(*   (\** [switch_i64 c ~cases ~default] constructs a jump table over [i64] values. *)
(*   *\) *)
(*   val switch_i64 : *)
(*     i64 m -> *)
(*     cases:(int64 * (unit -> 'a m)) array -> *)
(*     default:(unit -> 'a m) -> *)
(*     'a m *)

(*   (\** [global_array num arr] creates a global array containing elements of the *)
(*       numerical type [num]. *\) *)
(*   val global_array : *)
(*     (module Numerical with type t = 'a and type v = 'v) -> *)
(*     'v array -> *)
(*     ('a, [ `cst ]) arr m *)

(*   (\** [malloc ty] allocates a value of type [ty] on the heap and returns a *)
(*       pointer to that value. *\) *)
(*   val malloc : 'a typ -> 'a ptr m *)

(*   (\** [malloc_array ty numel] allocates an array with elements of type [ty] and *)
(*       [numel] elements on the heap. *\) *)
(*   val malloc_array : 'a typ -> i64 m -> ('a, [ `unk ]) arr m *)

(*   (\** [free ptr] frees some heap-allocated memory. Freeing an invalid pointer is *)
(*       undefined behaviour. *\) *)
(*   val free : 'a ptr m -> unit m *)

(*   (\** [free_array] frees a heap-allocated array. *\) *)
(*   val free_array : ('a, [ `unk ]) arr m -> unit m *)

(*   (\** The type of stack-allocating code. *\) *)
(*   type _ stack_allocating *)

(*   val ( let*:: ) : *)
(*     'a Stack.stack_var -> ('a m -> 'b stack_allocating) -> 'b stack_allocating *)

(*   val local : *)
(*     'a Stack.stack_var -> ('a m -> 'b stack_allocating) -> 'b stack_allocating *)

(*   val end_frame : 's -> 's stack_allocating *)

(*   (\** [fundecl name prototype body] defines a function [name] with arguments *)
(*       described by [prototype]. [body] takes as argument pointers to the data on *)
(*       the stack followed by the arguments described in the prototype. *\) *)
(*   val fundecl : *)
(*     string -> 's Types.fn -> ('s fundecl -> 's) stack_allocating -> 's fundecl k *)

(*   (\** [call fdecl] constructs a function call to [fdecl]. *\) *)
(*   val call : 's fundecl -> 's *)

(*   (\** [fail msg] raises [Failure msg] at code {b evaluation} time. *\) *)
(*   val fail : string -> 'a m *)

(*   (\** [print msg] prints [msg] to [stdout]. *\) *)
(*   val print : string -> unit m *)

(*   (\** See {!module-type-Cast}. *\) *)
(*   module Cast : Cast *)

(*   (\** {2 Executing code} *\) *)

(*   (\** [Exec] contains facilities to run code. *\) *)
(*   module Exec : sig *)
(*     open Types *)

(*     (\** [('s, 'o) rel] is the type of witnesses that {b suplex} expressions of *)
(*         type ['s] can be mapped to OCaml expressions of type ['o]. *)

(*         This type is used to describe how one feeds data to a *)
(*         {b suplex}-generated function. *\) *)
(*     type (_, _) rel *)

(*     (\** Relation vector. These are used to construct relations between *)
(*         {b suplex} records and OCaml values. *\) *)
(*     type (_, _) rel_vec *)

(*     (\** A relation between {b suplex} and OCaml function types. *\) *)
(*     type (_, _) fn_rel *)

(*     (\** ['a opaque] is used to hide the actual details of a {b suplex} record *)
(*         type on the OCaml side. *\) *)
(*     type _ opaque *)

(*     (\** {3 Feeding OCaml values to {b suplex}-generated functions and getting *)
(*         results back} *\) *)

(*     (\** [empty] is the empty relation. *\) *)
(*     val empty : (unit Vec.t, unit Vec.t) rel_vec *)

(*     (\** [vec |+ rel] extends the relation vector [vec] with [rel]. Note that *)
(*         this matches the syntax of record construction. *\) *)
(*     val ( |+ ) : *)
(*       ('c Vec.t, 'd Vec.t) rel_vec -> *)
(*       ('a m, 'b) rel -> *)
(*       (('a * 'c) Vec.t, ('b * 'd) Vec.t) rel_vec *)

(*     (\** [BA] is the module type of relations between {b suplex} arrays and *)
(*         bigarrays. *\) *)
(*     module type BA = sig *)
(*       (\** The type of elements in the array on the {b suplex} side. *\) *)
(*       type elt *)

(*       (\** The type of suplex 'big arrays' (see the record definition {!r} *)
(*           below). *\) *)
(*       type s *)

(*       (\** In {b suplex}, bigarrays are mapped to two-elements records that *)
(*           contain the length of the bigarray and a pointer to the data. *)

(*           The record definition [r] corresponds to the definition of this record *)
(*           type. *\) *)
(*       val r : *)
(*         ( (unit * (i64, s) proj) * ((elt, [ `unk ]) arr, s) proj, *)
(*           ((elt, [ `unk ]) arr * (i64 * unit)) Vec.t, *)
(*           ((elt, [ `unk ]) arr * (i64 * unit)) Vec.t, *)
(*           s ) *)
(*         record *)

(*       val s : s typ *)

(*       (\** [data] is a projection used to get the pointer to the data from a *)
(*           {b suplex} 'big array'. *\) *)
(*       val data : ((elt, [ `unk ]) arr, s) proj *)

(*       (\** [dim] is a projection used to get the number of elements from a *)
(*           {b suplex} 'big array' *\) *)
(*       val dim : (i64, s) proj *)
(*     end *)

(*     (\** [I64_ba] is the module allowing to access a bigarray containing 64-bit *)
(*         signed integers. *\) *)
(*     module I64_ba : BA with type elt = i64 *)

(*     (\** [I32_ba] is the module allowing to access a bigarray containing 32-bit *)
(*         signed integers. *\) *)
(*     module I32_ba : BA with type elt = i32 *)

(*     (\** [I16_ba] is the module allowing to access a bigarray containing 16-bit *)
(*         signed integers. *\) *)
(*     module I16_ba : BA with type elt = i16 *)

(*     (\** [I8_ba] is the module allowing to access a bigarray containing 8-bit *)
(*         signed integers. *\) *)
(*     module I8_ba : BA with type elt = i8 *)

(*     (\** [F64_ba] is the module allowing to access a bigarray containing *)
(*         double-precision floating point numbers. *\) *)
(*     module F64_ba : BA with type elt = f64 *)

(*     (\** [F32_ba] is the module allowing to access a bigarray containing *)
(*         double-precision floating point numbers. *\) *)
(*     module F32_ba : BA with type elt = f32 *)

(*     (\** [bigarray_i64] witnesses that [int64] bigarrays can mapped to *)
(*         [I64_ba.s]. *\) *)
(*     val bigarray_i64 : *)
(*       ( I64_ba.s ptr m, *)
(*         (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t ) *)
(*       rel *)

(*     (\** [bigarray_i32] witnesses that [int32] bigarrays can mapped to *)
(*         [I32_ba.s]. *\) *)
(*     val bigarray_i32 : *)
(*       ( I32_ba.s ptr m, *)
(*         (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t ) *)
(*       rel *)

(*     (\** [bigarray_i16] witnesses that [int16] bigarrays can mapped to *)
(*         [I16_ba.s]. *\) *)
(*     val bigarray_i16 : *)
(*       ( I16_ba.s ptr m, *)
(*         (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t *)
(*       ) *)
(*       rel *)

(*     (\** [bigarray_i8] witnesses that [int8] bigarrays can mapped to [I8_ba.s]. *)
(*     *\) *)
(*     val bigarray_i8 : *)
(*       ( I8_ba.s ptr m, *)
(*         (int, Bigarray.int8_signed_elt, Bigarray.c_layout) Bigarray.Array1.t ) *)
(*       rel *)

(*     (\** [bigarray_f64] witnesses that [float64] bigarrays can mapped to *)
(*         [F64_ba.s]. *\) *)
(*     val bigarray_f64 : *)
(*       ( F64_ba.s ptr m, *)
(*         (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ) *)
(*       rel *)

(*     (\** [bigarray_f32] witnesses that [float32] bigarrays can mapped to *)
(*         [F32_ba.s]. *\) *)
(*     val bigarray_f32 : *)
(*       ( F32_ba.s ptr m, *)
(*         (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ) *)
(*       rel *)

(*     (\** Basic relations. *\) *)

(*     val unit : (unit m, unit) rel *)

(*     val bool : (bool m, bool) rel *)

(*     val i64 : (i64 m, int64) rel *)

(*     val i32 : (i32 m, int32) rel *)

(*     val i16 : (i16 m, int) rel *)

(*     val i8 : (i8 m, int) rel *)

(*     val f64 : (f64 m, float) rel *)

(*     val f32 : (f32 m, float) rel *)

(*     val string : (i8 ptr m, string) rel *)

(*     val array_raw : ('a m, 'b) rel -> (('a, [ `unk ]) arr m, 'b Seq.t) rel *)

(*     val array : int -> ('a m, 'b) rel -> (('a, [ `cst ]) arr m, 'b Seq.t) rel *)

(*     val mallocd_strct : *)
(*       ('a, 'b Vec.t, 'b Vec.t, 'c) record -> *)
(*       ('b Vec.t, 'd Vec.t) rel_vec -> *)
(*       ('c ptr m, 'd Vec.t) rel *)

(*     val opaque_mallocd_strct : *)
(*       ('a, 'b Vec.t, 'b Vec.t, 'c) record -> ('c ptr m, 'c opaque) rel *)

(*     val strct : *)
(*       ('a, 'b Vec.t, 'b Vec.t, 'c) record -> *)
(*       ('b Vec.t, 'd Vec.t) rel_vec -> *)
(*       ('c m, 'd Vec.t) rel *)

(*     val returning : ('a m, 'b) rel -> ('a m, 'b) fn_rel *)

(*     val ( @-> ) : *)
(*       ('a m, 'b) rel -> ('c, 'd) fn_rel -> ('a m -> 'c, 'b -> 'd) fn_rel *)

(*     (\** {3 Running suplex computations} *\) *)

(*     (\** [cfg] is the type of the configuration used by the underlying code *)
(*         generation (e.g. LLVM). *\) *)
(*     type cfg *)

(*     (\** ['a module_] is the type of {b suplex} modules, consisting of a vector *)
(*         of functions. *\) *)
(*     type _ module_ *)

(*     (\** [fdecl f frel] creates a module with a function declaration [f] with *)
(*         OCaml type defined by [frel]. *\) *)
(*     (\* val fdecl : *)
(*      *   's fundecl -> ('s, 'dom -> 'range) fn_rel -> ('dom -> 'range) module_ *\) *)

(*     val empty_module : unit module_ *)

(*     (\** [add_fdecl f frel mdl] extends the module [mdl] with a function *)
(*         declaration [f] with OCaml type defined by [frel]. *\) *)
(*     val add_fdecl : *)
(*       's fundecl -> *)
(*       ('s, 'dom -> 'range) fn_rel -> *)
(*       'r module_ -> *)
(*       ('r * ('dom -> 'range)) module_ *)

(*     (\** [lookup_function mdl fname fsig] lookups a function named [fname] with *)
(*         signature equal [fsig] in [mdl]. *\) *)
(*     val lookup_function : *)
(*       's module_ -> string -> 'f Types.fn -> 'f fundecl option *)

(*     (\** [run_module ?cfg mdl] evaluates [mdl] and returns the evaluated module. *)
(*     *\) *)
(*     val run_module : ?cfg:cfg -> 'a module_ k -> 'a *)

(*     (\** [run_program] is as [run_module] for a single function. *\) *)
(*     val run_program : *)
(*       ?cfg:cfg -> 'a fundecl k -> ('a, 'b -> 'c) fn_rel -> 'b -> 'c *)

(*     (\** [run ?cfg ?fname rel body] is as [run_program] but automatically derives *)
(*         the prototype from [rel]. *\) *)
(*     val run : *)
(*       ?cfg:cfg -> *)
(*       ?fname:string -> *)
(*       ('f, 'b -> 'c) fn_rel -> *)
(*       ('f fundecl -> 'f) stack_allocating -> *)
(*       'b -> *)
(*       'c *)
(*   end *)
(* end *)
