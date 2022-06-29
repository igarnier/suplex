type _ vec = Nil_vec : unit vec | Cons_vec : 'a * 'b vec -> ('a * 'b) vec

type const = [ `const ]

type not_const = [ `not_const ]

type unknown = [ `const | `not_const ]

module type Type_system_sig = sig
  type (!'a, 'a_typ, +'c) typed_term

  type 'a numerical = ..

  type 'a numerical += Int64_num : int64 numerical

  type ex_numerical = Ex_num : 'a numerical -> ex_numerical

  type numerical_info =
    | Pp :
        { pred : ex_numerical -> 't option;
          pp : Format.formatter -> 't -> unit;
          kind : [ `fp | `int ]
        }
        -> numerical_info

  type 'a typ =
    | TUnit : unit typ
    | TBool : bool typ
    | TNum : 'a numerical -> 'a typ
    | TPtr : 'a typ -> 'a ref typ
    | TVec : int option * 'a typ -> 'a array typ
    | TRecord : (_, _, 'u vec, 'u vec, 't) record -> 't typ

  and ('intro, 'elim, 't_acc, 't, 'u) record =
    | Record_empty : (('u, const) m, unit, unit vec, 't vec, 'u) record
    | Record_field :
        ('a, 'u) field * ('i, 'e, 't_acc vec, 't vec, 'u) record
        -> ( ('a, const) m -> 'i,
             'e * ('a, 'u) proj,
             ('a * 't_acc) vec,
             't vec,
             'u )
           record
    | Record_fix :
        int * ('u typ -> ('a, 'b, 'd vec, 'd vec, 'u) record)
        -> ('a, 'b, 'd vec, 'd vec, 'u) record

  and ('a, 't) field = { name : string; ty : 'a typ }

  and ('a, 'u) proj = { proj : 'c. ('u ref, 'c) m -> ('a ref, 'c) m }

  and (!'a, +'c) m = ('a, 'a typ, 'c) typed_term

  val register_numerical :
    (ex_numerical -> 'a option) ->
    (Format.formatter -> 'a -> unit) ->
    [ `fp | `int ] ->
    unit

  val pp_numerical : Format.formatter -> 'a numerical -> unit

  val numerical_kind : 't numerical -> [ `fp | `int ]

  type ('a, 'b) eq = Refl_eq : ('a, 'a) eq

  val pp_typ : Format.formatter -> 'a typ -> unit

  val unit : unit typ

  val bool : bool typ

  val int64 : int64 typ

  val ptr : 'a typ -> 'a ref typ

  val vec : ?static_size:int -> 'a typ -> 'a array typ

  val empty_rec : (('u, const) m, unit, unit vec, 't vec, 'u) record

  val field : string -> 'a typ -> ('a, 'u) field

  val ( |+ ) :
    ('a, 'b, 'c vec, 'd vec, 'e) record ->
    ('f, 'e) field ->
    (('f, const) m -> 'a, 'b * ('f, 'e) proj, ('f * 'c) vec, 'd vec, 'e) record

  val fix :
    ('u typ -> ('a, 'b, 'd vec, 'd vec, 'u) record) ->
    ('a, 'b, 'd vec, 'd vec, 'u) record

  val seal : ('a, 'b, 'd vec, 'd vec, 'u) record -> 'u typ
end

module Make_type_system (M : sig
  type (!'a, 'a_typ, +'c) typed_term
end) :
  Type_system_sig with type ('a, 'b, 'c) typed_term = ('a, 'b, 'c) M.typed_term =
struct
  type ('a, 'b, 'c) typed_term = ('a, 'b, 'c) M.typed_term

  type 'a numerical = ..

  type 'a numerical += Int64_num : int64 numerical

  type ex_numerical = Ex_num : 'a numerical -> ex_numerical

  type numerical_info =
    | Pp :
        { pred : ex_numerical -> 't option;
          pp : Format.formatter -> 't -> unit;
          kind : [ `fp | `int ]
        }
        -> numerical_info

  type 'a typ =
    | TUnit : unit typ
    | TBool : bool typ
    | TNum : 'a numerical -> 'a typ
    | TPtr : 'a typ -> 'a ref typ
    | TVec : int option * 'a typ -> 'a array typ
    | TRecord : (_, _, 'u vec, 'u vec, 't) record -> 't typ

  and ('intro, 'elim, 't_acc, 't, 'u) record =
    | Record_empty : (('u, const) m, unit, unit vec, 't vec, 'u) record
    | Record_field :
        ('a, 'u) field * ('i, 'e, 't_acc vec, 't vec, 'u) record
        -> ( ('a, const) m -> 'i,
             'e * ('a, 'u) proj,
             ('a * 't_acc) vec,
             't vec,
             'u )
           record
    | Record_fix :
        int * ('u typ -> ('a, 'b, 'd vec, 'd vec, 'u) record)
        -> ('a, 'b, 'd vec, 'd vec, 'u) record

  and ('a, 't) field = { name : string; ty : 'a typ }

  and ('a, 'u) proj = { proj : 'c. ('u ref, 'c) m -> ('a ref, 'c) m }

  and (!'a, +'c) m = ('a, 'a typ, 'c) typed_term

  let table = ref []

  let register_numerical pred pp kind = table := Pp { pred; pp; kind } :: !table

  let () =
    register_numerical
      (function Ex_num Int64_num -> Some () | _ -> None)
      (fun fmtr () -> Format.fprintf fmtr "int64")
      `int

  let pp_numerical : type a. Format.formatter -> a numerical -> unit =
    fun (type a) fmtr (n : a numerical) ->
     let rec loop list num =
       match list with
       | [] -> ()
       | Pp { pred; pp; kind = _ } :: tl -> (
           match pred num with None -> loop tl num | Some x -> pp fmtr x)
     in
     loop !table (Ex_num n)

  let numerical_kind (type t) (n : t numerical) =
    let rec loop list num =
      match list with
      | [] -> invalid_arg "numerical_kind: unregistered input"
      | Pp { pred; kind; pp = _ } :: tl -> (
          match pred num with None -> loop tl num | Some _ -> kind)
    in
    loop !table (Ex_num n)

  type ('a, 'b) eq = Refl_eq : ('a, 'a) eq

  let rec pp_typ : type a. int list -> Format.formatter -> a typ -> unit =
    fun (type a) visited fmtr (typ : a typ) ->
     match typ with
     | TUnit -> Format.pp_print_string fmtr "unit"
     | TBool -> Format.pp_print_string fmtr "bool"
     | TNum n -> pp_numerical fmtr n
     | TPtr t -> Format.fprintf fmtr "[%a]" (pp_typ visited) t
     | TVec (static_size, t) -> (
         match static_size with
         | None -> Format.fprintf fmtr "<%a>" (pp_typ visited) t
         | Some sz -> Format.fprintf fmtr "<%a:%d>" (pp_typ visited) t sz)
     | TRecord descr ->
         let rec loop :
             type w x y z.
             int list -> Format.formatter -> (w, x, y, z, a) record -> unit =
          fun visited fmtr descr ->
           match descr with
           | Record_empty -> ()
           | Record_field (field, rest) ->
               Format.fprintf fmtr "%s: %a" field.name (pp_typ visited) field.ty ;
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

  let unit = TUnit

  let bool = TBool

  let int64 = TNum Int64_num

  let ptr x = TPtr x

  let vec ?static_size x =
    Option.iter
      (fun sz ->
        if sz < 0 then invalid_arg "Type_system.vec: negative static size")
      static_size ;
    TVec (static_size, x)

  let empty_rec = Record_empty

  let field name ty = { name; ty }

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
end

module type Numerical = sig
  type !'a typ

  type (!'a, 'c) m

  type t

  type v

  val t : t typ

  val v : v -> (t, const) m

  val add : (t, _) m -> (t, _) m -> (t, unknown) m

  val sub : (t, _) m -> (t, _) m -> (t, unknown) m

  val mul : (t, _) m -> (t, _) m -> (t, unknown) m

  val div : (t, _) m -> (t, _) m -> (t, unknown) m

  val neg : (t, _) m -> (t, unknown) m

  val lt : (t, _) m -> (t, _) m -> (bool, unknown) m

  val le : (t, _) m -> (t, _) m -> (bool, unknown) m

  val eq : (t, _) m -> (t, _) m -> (bool, unknown) m
end

module type Stack_frame = sig
  type !'a typ

  type (!'a, 'c) m

  type 'a stack_var =
    | Single : 'a typ * ('a, const) m -> 'a ref stack_var
    | Array : 'a typ * ('a, const) m * (int64, unknown) m -> 'a array stack_var

  type (_, _) t =
    | Empty : ('b, 'b) t
    | Cons : 'a stack_var * ('c, 'b) t -> (('a, not_const) m -> 'c, 'b) t

  val empty : ('b, 'b) t

  val ( @+ ) : 'a stack_var -> ('c, 'b) t -> (('a, not_const) m -> 'c, 'b) t

  val single : 'a typ -> ('a, const) m -> 'a ref stack_var

  val array_i64 :
    'a typ -> ('a, const) m -> (int64, unknown) m -> 'a array stack_var
end

module type Prototype = sig
  type !'a typ

  type (!'a, 'c) m

  type (_, _) t =
    | Returning : 'a typ -> (unit, ('a, unknown) m) t
    | Arrow :
        'a typ * ('d, ('ret, unknown) m) t
        -> (('a, 'c) m * 'd, ('ret, unknown) m) t

  val returning : 'a typ -> (unit, ('a, unknown) m) t

  val ( @-> ) :
    'a typ ->
    ('b, ('ret, unknown) m) t ->
    (('a, 'c) m * 'b, ('ret, unknown) m) t

  type _ args =
    | [] : unit args
    | ( :: ) : ('a, 'c) m * 'd args -> (('a, 'c) m * 'd) args
end

module type S = sig
  type +!'a k

  type (!'a, 'typ, +'c) expr

  module Type_system :
    Type_system_sig with type ('a, 'typ, 'c) typed_term = ('a, 'typ, 'c) expr k

  type ('a, 'c) m := ('a, 'c) Type_system.m

  type 'a typ := 'a Type_system.typ

  module type Numerical =
    Numerical with type 'a typ := 'a typ and type ('a, 'c) m := ('a, 'c) m

  module Stack_frame :
    Stack_frame with type 'a typ := 'a typ and type ('a, 'c) m := ('a, 'c) m

  module Prototype :
    Prototype with type 'a typ := 'a typ and type ('a, 'c) m := ('a, 'c) m

  type ('a, 'b) fundecl

  val unit : (unit, const) m

  val tt : (bool, const) m

  val ff : (bool, const) m

  val ( && ) : (bool, _) m -> (bool, _) m -> (bool, unknown) m

  val ( || ) : (bool, _) m -> (bool, _) m -> (bool, unknown) m

  module I64 : Numerical with type t = int64 and type v = int64

  val array : ('a, const) m array -> ('a array, const) m

  val struct_ :
    ('intro, _, 'a vec, 'a vec, 'u) Type_system.record ->
    ('a vec -> 'u) ->
    'intro

  val projs :
    (_, 'elim, 'a vec, 'a vec, 'u) Type_system.record -> ('u -> 'a vec) -> 'elim

  val seq : (unit, _) m -> (unit -> ('a, unknown) m) -> ('a, unknown) m

  val ( let* ) :
    ('a, 'c) m -> (('a, 'c) m -> ('b, unknown) m) -> ('b, unknown) m

  val store : ('a ref, not_const) m -> ('a, _) m -> (unit, unknown) m

  val load : ('a ref, 'c) m -> ('a, 'c) m

  val get : ('a array, 'c) m -> (int64, _) m -> ('a, 'c) m

  val set :
    ('a array, not_const) m -> (int64, _) m -> ('a, _) m -> (unit, unknown) m

  val for_ :
    init:(int64, 'a) m ->
    pred:((int64, 'b) m -> (bool, 'c) m) ->
    step:((int64, 'b) m -> (int64, 'd) m) ->
    ((int64, 'b) m -> (unit, 'e) m) ->
    (unit, unknown) m

  val cond : (bool, _) m -> (bool -> ('a, _) m) -> ('a, unknown) m

  val switch_i64 :
    (int64, _) m ->
    cases:(int64 * (unit -> ('a, _) m)) array ->
    default:(unit -> ('a, _) m) ->
    ('a, unknown) m

  val fundecl :
    name:string ->
    signature:('s, ('ret, unknown) m) Prototype.t ->
    local:('b, 's -> ('ret, unknown) m) Stack_frame.t ->
    body:'b ->
    ('s, ('ret, unknown) m) fundecl k

  val call : ('s, 'ret) fundecl -> 's Prototype.args -> 'ret
end

module Stack_frame (E : sig
  type !'a typ

  type (!'a, 'c) m
end) :
  Stack_frame with type !'a typ := 'a E.typ and type ('a, 'c) m := ('a, 'c) E.m =
struct
  open E

  type 'a stack_var =
    | Single : 'a typ * ('a, const) m -> 'a ref stack_var
    | Array : 'a typ * ('a, const) m * (int64, unknown) m -> 'a array stack_var

  type (_, _) t =
    | Empty : ('b, 'b) t
    | Cons : 'a stack_var * ('c, 'b) t -> (('a, not_const) m -> 'c, 'b) t

  let empty = Empty

  let ( @+ ) v f = Cons (v, f)

  let single ty init = Single (ty, init)

  let array_i64 ty init len = Array (ty, init, len)
end

module Prototype (E : sig
  type !'a typ

  type (!'a, 'c) m
end) :
  Prototype with type 'a typ := 'a E.typ and type ('a, 'c) m := ('a, 'c) E.m =
struct
  open E

  type (_, _) t =
    | Returning : 'a typ -> (unit, ('a, unknown) m) t
    | Arrow :
        'a typ * ('d, ('ret, unknown) m) t
        -> (('a, 'c) m * 'd, ('ret, unknown) m) t

  let returning ty = Returning ty

  let ( @-> ) ty arr = Arrow (ty, arr)

  type _ args =
    | [] : unit args
    | ( :: ) : ('a, 'c) m * 'd args -> (('a, 'c) m * 'd) args
end

module OCaml_repr () : sig
  type (!'a, _, 'c) expr = 'a

  include
    S with type ('a, 'typ, 'c) expr := ('a, 'typ, 'c) expr and type 'a k = 'a

  module I32 : Numerical with type t = int32 and type v = int32

  module F64 : Numerical with type t = float and type v = float
end = struct
  module Type_system : Type_system_sig with type ('a, 'b, 'c) typed_term = 'a =
  Make_type_system (struct
    type ('a, 'b, 'c) typed_term = 'a
  end)

  open Type_system

  type !'a k = 'a

  type (!'a, _, _) expr = 'a

  type ('a, +'c) m = ('a, 'c) Type_system.m

  module type Numerical =
    Numerical with type 'a typ := 'a typ and type ('a, 'c) m := ('a, 'c) m

  module Stack_frame = Stack_frame (struct
    type nonrec !'a typ = 'a typ

    type (!'a, 'c) m = 'a
  end)

  module Prototype = Prototype (struct
    type nonrec !'a typ = 'a typ

    type nonrec (!'a, 'c) m = 'a
  end)

  type !'a numerical +=
    | Int32_num : int32 numerical
    | Float64_num : float numerical
    [@@ocaml.warning "-38"]

  let () =
    register_numerical
      (function Ex_num Int32_num -> Some () | _ -> None)
      (fun fmtr () -> Format.fprintf fmtr "int32")
      `int ;
    register_numerical
      (function Ex_num Float64_num -> Some () | _ -> None)
      (fun fmtr () -> Format.fprintf fmtr "float")
      `fp

  type ('a, 'ret) fundecl = 'a -> 'ret

  let unit = ()

  let seq (_m : (unit, _) m) (f : unit -> ('b, _) m) = f ()

  let ( let* ) m f = f m

  let bool b = b

  let tt = bool true

  let ff = bool false

  let ( && ) : (bool, _) m -> (bool, _) m -> (bool, unknown) m =
   fun x y -> x && y

  let ( || ) : (bool, _) m -> (bool, _) m -> (bool, unknown) m =
   fun x y -> x || y

  module I64 : Numerical with type t = int64 and type v = int64 = struct
    type t = int64

    type v = int64

    let t = Type_system.int64

    let v i = i

    let add = Int64.add

    let sub = Int64.sub

    let mul = Int64.mul

    let div = Int64.div

    let neg = Int64.neg

    let lt = ( < )

    let le = ( <= )

    let eq = ( = )
  end

  module I32 : Numerical with type t = int32 and type v = int32 = struct
    type t = int32

    type v = int32

    let t = Type_system.TNum Int32_num

    let v i = i

    let add = Int32.add

    let sub = Int32.sub

    let mul = Int32.mul

    let div = Int32.div

    let neg = Int32.neg

    let lt = ( < )

    let le = ( <= )

    let eq = ( = )
  end

  module F64 : Numerical with type t = float and type v = float = struct
    type t = float

    type v = float

    let t = Type_system.TNum Float64_num

    let v f = f

    let add = ( +. )

    let sub = ( -. )

    let mul = ( *. )

    let div = ( /. )

    let neg = ( ~-. )

    let lt = ( < )

    let le = ( <= )

    let eq = ( = )
  end

  let store lhs rhs = lhs := rhs

  let load ptr = !ptr

  let get vec index = vec.(Int64.to_int index)

  let set vec index elt = vec.(Int64.to_int index) <- elt

  let for_ ~init ~pred ~step body =
    let i_ref = ref init in
    while pred !i_ref do
      body !i_ref ;
      i_ref := step !i_ref
    done

  let cond e dispatch = dispatch e

  let switch_i64 expr ~cases ~default =
    let rec loop i =
      if i = Array.length cases then default ()
      else
        let (v, code) = cases.(i) in
        if v = expr then code () else loop (i + 1)
    in
    loop 0

  let rec alloca : type a b. (a, b) Stack_frame.t -> a -> b =
   fun frame k ->
    match frame with
    | Empty -> k
    | Cons (Single (_typ, init), rest) ->
        let expr = ref init in
        alloca rest (k expr)
    | Cons (Array (_typ, init, size), rest) ->
        let expr = Array.make (Int64.to_int size) init in
        alloca rest (k expr)

  let fundecl ~name:_ ~signature:_ ~local ~body = alloca local body

  let rec call : type s ret. (s, ret) fundecl -> s Prototype.args -> ret =
   fun f args ->
    match args with
    | [] -> f ()
    | expr :: args -> call (fun x -> f (expr, x)) args

  let array arr = arr

  let struct_ :
      type intro res u.
      (intro, _, res vec, res vec, u) record -> (res vec -> u) -> intro =
   fun (record : (intro, _, res vec, res vec, u) record) conv ->
    let rec loop :
        type intro elim acc.
        (intro, elim, acc vec, res vec, u) Type_system.record ->
        (acc vec -> res vec) ->
        intro =
     fun (descr : (intro, elim, acc vec, res vec, u) Type_system.record) k ->
      match descr with
      | Record_empty -> conv (k Nil_vec)
      | Record_field (_field, rest) ->
          fun arg -> loop rest (fun x -> k (Cons_vec (arg, x)))
      | Record_fix (_id, f) ->
          (* Can only appear at top-level *)
          loop (f (seal descr)) k
    in
    loop record (fun x -> x)

  module Crazy_hack = struct
    (* I just don't understand *)

    type ('a, 'b) u = 'a

    type ('a, 't) local_proj = { f : 'c. ('t ref, 'c) u -> ('a ref, 'c) u }

    let local_to_proj : ('a, 'b) local_proj -> ('a, 'b) proj = Obj.magic
  end

  let projs :
      type elim res u.
      (_, elim, res vec, res vec, u) record -> (u -> res vec) -> elim =
   fun (record : (_, elim, res vec, res vec, _) record) conv ->
    let rec loop :
        type intro elim acc.
        (intro, elim, acc vec, res vec, u) Type_system.record ->
        (res vec -> acc vec) ->
        elim =
     fun descr prj ->
      match descr with
      | Record_empty -> ()
      | Record_field (_field, rest) ->
          let proj : (_, u) proj =
            (* Without this crazy hack OCaml can't generalize as it should. *)
            Crazy_hack.local_to_proj
              { f =
                  (fun record ->
                    match prj (conv !record) with Cons_vec (x, _) -> ref x)
              }
          in
          let elims =
            loop rest (fun vec -> match prj vec with Cons_vec (_, tl) -> tl)
          in
          ((elims, proj) : elim)
      | Record_fix (_id, f) -> loop (f (seal descr)) prj
    in
    loop record (fun x -> x)
end

let _fact_example () =
  let open OCaml_repr () in
  let fact =
    fundecl
      ~name:"fact"
      ~signature:Prototype.(Type_system.int64 @-> returning Type_system.int64)
      ~local:Stack_frame.(single Type_system.int64 (I64.v 1L) @+ empty)
      ~body:(fun acc (n, ()) ->
        let* _ =
          for_
            ~init:(I64.v 1L)
            ~pred:(fun i -> I64.le i n)
            ~step:(fun i -> I64.add i (I64.v 1L))
            (fun i -> store acc (I64.mul (load acc) i))
        in
        load acc)
  in
  let res = call fact Prototype.[I64.v 5L] in
  Format.printf "fact 5 = %Ld@." res

module SMap = Map.Make (String)

module LLVM_repr () : sig
  module LLVM_state : sig
    type +!'a t

    val return : 'a -> 'a t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    type llvm_state =
      { llvm_context : Llvm.llcontext;
        llvm_module : Llvm.llmodule;
        llvm_builder : Llvm.llbuilder;
        externals : Llvm.llvalue SMap.t
      }

    val run : 'a t -> llvm_state * 'a
  end

  include S with type 'a k = 'a LLVM_state.t

  val null_ptr : 'a Type_system.typ -> ('a ref, const) Type_system.m

  module I32 : Numerical with type t = int32 and type v = int32

  module F64 : Numerical with type t = float and type v = float
end = struct
  type ('a, 'typ, 'c) expr = { llval : Llvm.llvalue; typewit : 'typ }

  module LLVM_state : sig
    type ('a, 'typ, 'c) llvm = ('a, 'typ, 'c) expr

    type +!'a t

    val return : 'a -> 'a t

    val llreturn : Llvm.llvalue -> 'typ -> ('a, 'typ, 'c) llvm t

    val llval : ('a, _, _) llvm -> Llvm.llvalue

    val typeof : (_, 'typ, _) llvm -> 'typ

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val lmodule : Llvm.llmodule t

    val context : Llvm.llcontext t

    val builder : Llvm.llbuilder t

    val externals : Llvm.llvalue SMap.t t [@@ocaml.warning "-32"]

    val set_externals : Llvm.llvalue SMap.t -> unit t [@@ocaml.warning "-32"]

    val dump_module : unit t [@@ocaml.warning "-32"]

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

    type ('a, 'typ, 'c) llvm = ('a, 'typ, 'c) expr

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

  module Type_system : sig
    include
      Type_system_sig
        with type ('a, 'b, 'c) typed_term = ('a, 'b, 'c) expr LLVM_state.t

    type !'a numerical +=
      | Int32_num : int32 numerical
      | Float64_num : float numerical

    val int32 : int32 typ

    val float64 : float typ
  end = struct
    include Make_type_system (struct
      type ('a, 'b, 'c) typed_term = ('a, 'b, 'c) expr LLVM_state.t
    end)

    type float32 = Float32 of float [@@unboxed] [@@ocaml.warning "-37"]

    type !'a numerical +=
      | Int32_num : int32 numerical
      | Float32_num : float32 numerical
      | Float64_num : float numerical
      [@@ocaml.warning "-38"]

    let int32 = TNum Int32_num

    let float64 = TNum Float64_num

    let () =
      register_numerical
        (function Ex_num Int32_num -> Some () | _ -> None)
        (fun fmtr () -> Format.fprintf fmtr "int32")
        `int ;
      register_numerical
        (function Ex_num Float64_num -> Some () | _ -> None)
        (fun fmtr () -> Format.fprintf fmtr "double")
        `fp
  end

  open Type_system

  type nonrec !'a typ = 'a typ

  type 'a k = 'a LLVM_state.t

  module LLVM_type = struct
    type t = Llvm.lltype

    let _void ctxt = Llvm.void_type ctxt

    let int64_t ctxt = Llvm.i64_type ctxt

    let int32_t ctxt = Llvm.i32_type ctxt

    let int16_t ctxt = Llvm.i16_type ctxt [@@ocaml.warning "-32"]

    let int8_t ctxt = Llvm.i8_type ctxt [@@ocaml.warning "-32"]

    let size_t ctxt = Llvm.i64_type ctxt [@@ocaml.warning "-32"]

    let bool_t ctxt = Llvm.i1_type ctxt

    let float64_t ctxt = Llvm.double_type ctxt

    let struct_table = Hashtbl.create 11

    (* Convert type to Llvm repr *)
    let rec of_type : type a. a Type_system.typ -> t LLVM_state.t =
      fun (type a) (typ : a Type_system.typ) : t LLVM_state.t ->
       let open LLVM_state in
       let* context in
       match typ with
       | TNum typ -> of_numerical typ
       | TUnit -> return (int8_t context)
       | TBool -> return (bool_t context)
       | TPtr typ ->
           let* lltyp = of_type typ in
           return (Llvm.pointer_type lltyp)
       | TVec (static_size, typ) -> (
           match static_size with
           | None ->
               let* lltyp = of_type typ in
               return (Llvm.pointer_type lltyp)
           | Some sz when sz >= 0 ->
               let* lltyp = of_type typ in
               return (Llvm.array_type lltyp sz)
           | _ -> failwith "LLVM_type.of_type: negative size in array type")
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

    and of_numerical : type a. a Type_system.numerical -> t LLVM_state.t =
      fun (type a) (typ : a Type_system.numerical) : Llvm.lltype LLVM_state.t ->
       let open LLVM_state in
       let open Type_system in
       let* context in
       match typ with
       | Int64_num -> return (int64_t context)
       | Int32_num -> return (int32_t context)
       | Float64_num -> return (float64_t context)
       | _ -> assert false

    and struct_of_tuple :
        type a b c d u.
        (a, b, c, d, u) Type_system.record -> _ -> t LLVM_state.t =
     fun descr k ->
      let open LLVM_state in
      let rec loop :
          type a b c d u.
          (a, b, c, d, u) Type_system.record ->
          Llvm.lltype list ->
          (Llvm.lltype array -> Llvm.lltype k) ->
          Llvm.lltype LLVM_state.t =
       fun descr acc k ->
        match descr with
        | Type_system.Record_empty ->
            let fields = List.rev acc in
            k (Array.of_list fields)
        | Type_system.Record_field (field, rest) ->
            let* typ = of_type field.ty in
            loop rest (typ :: acc) k
        | Type_system.Record_fix (id, f) ->
            let unfolded = f (seal descr) in
            assert (acc = []) ;
            assert (Hashtbl.mem struct_table id) ;
            loop unfolded acc k
      in
      loop descr [] k

    let of_type ty =
      let open LLVM_state in
      let* res = of_type ty in
      Format.printf
        "us: %a llvm: %s@."
        Type_system.pp_typ
        ty
        (Llvm.string_of_lltype res) ;
      return res
  end

  module type Numerical =
    Numerical with type 'a typ := 'a typ and type ('a, 'c) m := ('a, 'c) m

  module Stack_frame = Stack_frame (struct
    type nonrec 'a typ = 'a typ

    type nonrec ('a, 'c) m = ('a, 'c) m
  end)

  module Prototype = Prototype (struct
    type nonrec 'a typ = 'a typ

    type nonrec ('a, 'c) m = ('a, 'c) m
  end)

  type ('s, 'ret) fundecl =
    { name : string; signature : ('s, 'ret) Prototype.t; fptr : Llvm.llvalue }

  let unit : (unit, const) m =
    let open LLVM_state in
    let* context in
    llreturn (Llvm.const_int (LLVM_type.int8_t context) 0) Type_system.unit

  let unit_unknown : (unit, unknown) m =
    let open LLVM_state in
    let* context in
    llreturn (Llvm.const_int (LLVM_type.int8_t context) 0) Type_system.unit

  let tt =
    let open LLVM_state in
    let* context in
    llreturn (Llvm.const_int (LLVM_type.bool_t context) 1) Type_system.bool

  let ff =
    let open LLVM_state in
    let* context in
    llreturn (Llvm.const_int (LLVM_type.bool_t context) 0) Type_system.bool

  let binop : type a. _ -> string -> (a, _) m -> (a, _) m -> (a, unknown) m =
   fun op name lhs rhs ->
    let open LLVM_state in
    let* builder in
    let* lhs in
    let* rhs in
    let typ = typeof lhs in
    let llval = op (llval lhs) (llval rhs) name builder in
    llreturn llval typ

  let bool_binop :
      type a. _ -> string -> (a, _) m -> (a, _) m -> (bool, unknown) m =
   fun op name lhs rhs ->
    let open LLVM_state in
    let* builder in
    let* lhs in
    let* rhs in
    let llval = op (llval lhs) (llval rhs) name builder in
    llreturn llval bool

  let null_ptr : 'a typ -> ('a ref, const) Type_system.m =
    let open LLVM_state in
    fun ty ->
      let ptrty = Type_system.ptr ty in
      let* llty = LLVM_type.of_type ptrty in
      let llval = Llvm.const_null llty in
      llreturn llval ptrty

  module I64 : Numerical with type t = int64 and type v = int64 = struct
    type t = int64

    type v = int64

    let t = Type_system.int64

    let v i =
      let open LLVM_state in
      let* context in
      llreturn
        (Llvm.const_int (LLVM_type.int64_t context) (Int64.to_int i))
        Type_system.int64

    let add : (int64, _) m -> (int64, _) m -> (int64, unknown) m =
     fun lhs rhs -> binop Llvm.build_add "int64_add" lhs rhs

    let sub : (int64, _) m -> (int64, _) m -> (int64, unknown) m =
     fun lhs rhs -> binop Llvm.build_sub "int64_sub" lhs rhs

    let mul : (int64, _) m -> (int64, _) m -> (int64, unknown) m =
     fun lhs rhs -> binop Llvm.build_mul "int64_mul" lhs rhs

    let div : (int64, _) m -> (int64, _) m -> (int64, unknown) m =
     fun lhs rhs -> binop Llvm.build_sdiv "int64_div" lhs rhs

    let neg : (int64, _) m -> (int64, unknown) m = fun x -> sub (v 0L) x

    let lt : (int64, _) m -> (int64, _) m -> (bool, unknown) m =
     fun lhs rhs ->
      bool_binop (Llvm.build_icmp Llvm.Icmp.Slt) "int64_lt" lhs rhs

    let le : (int64, _) m -> (int64, _) m -> (bool, unknown) m =
     fun lhs rhs ->
      bool_binop (Llvm.build_icmp Llvm.Icmp.Sle) "int64_le" lhs rhs

    let eq : (int64, _) m -> (int64, _) m -> (bool, unknown) m =
     fun lhs rhs -> bool_binop (Llvm.build_icmp Llvm.Icmp.Eq) "int64_eq" lhs rhs
  end

  module I32 : Numerical with type t = int32 and type v = int32 = struct
    type t = int32

    type v = int32

    type nonrec bool = bool

    let t = Type_system.int32

    let v i =
      let open LLVM_state in
      let* context in
      llreturn
        (Llvm.const_int (LLVM_type.int32_t context) (Int32.to_int i))
        Type_system.int32

    let add : (int32, _) m -> (int32, _) m -> (int32, unknown) m =
     fun lhs rhs -> binop Llvm.build_add "int32_add" lhs rhs

    let sub : (int32, _) m -> (int32, _) m -> (int32, unknown) m =
     fun lhs rhs -> binop Llvm.build_sub "int32_sub" lhs rhs

    let mul : (int32, _) m -> (int32, _) m -> (int32, unknown) m =
     fun lhs rhs -> binop Llvm.build_mul "int32_mul" lhs rhs

    let div : (int32, _) m -> (int32, _) m -> (int32, unknown) m =
     fun lhs rhs -> binop Llvm.build_sdiv "int32_div" lhs rhs

    let neg : (int32, _) m -> (int32, unknown) m = fun x -> sub (v 0l) x

    let lt : (int32, _) m -> (int32, _) m -> (bool, unknown) m =
     fun lhs rhs ->
      bool_binop (Llvm.build_icmp Llvm.Icmp.Slt) "int32_lt" lhs rhs

    let le : (int32, _) m -> (int32, _) m -> (bool, unknown) m =
     fun lhs rhs ->
      bool_binop (Llvm.build_icmp Llvm.Icmp.Sle) "int32_le" lhs rhs

    let eq : (int32, _) m -> (int32, _) m -> (bool, unknown) m =
     fun lhs rhs -> bool_binop (Llvm.build_icmp Llvm.Icmp.Eq) "int32_eq" lhs rhs
  end

  module F64 : Numerical with type t = float and type v = float = struct
    type t = float

    type v = float

    type nonrec bool = bool

    let t = Type_system.float64

    let v f =
      let open LLVM_state in
      let* context in
      llreturn
        (Llvm.const_float (LLVM_type.float64_t context) f)
        Type_system.float64

    let add : (float, _) m -> (float, _) m -> (float, unknown) m =
     fun lhs rhs -> binop Llvm.build_fadd "float_add" lhs rhs

    let sub : (float, _) m -> (float, _) m -> (float, unknown) m =
     fun lhs rhs -> binop Llvm.build_fsub "float_sub" lhs rhs

    let mul : (float, _) m -> (float, _) m -> (float, unknown) m =
     fun lhs rhs -> binop Llvm.build_fmul "float_mul" lhs rhs

    let div : (float, _) m -> (float, _) m -> (float, unknown) m =
     fun lhs rhs -> binop Llvm.build_fdiv "float_div" lhs rhs

    let neg : (float, _) m -> (float, unknown) m =
     fun x ->
      let open LLVM_state in
      let* builder in
      let* x in
      llreturn
        (Llvm.build_fneg (llval x) "float_fneg" builder)
        Type_system.float64

    let lt : (float, _) m -> (float, _) m -> (bool, unknown) m =
     fun lhs rhs ->
      bool_binop (Llvm.build_fcmp Llvm.Fcmp.Olt) "float_lt" lhs rhs

    let le : (float, _) m -> (float, _) m -> (bool, unknown) m =
     fun lhs rhs ->
      bool_binop (Llvm.build_fcmp Llvm.Fcmp.Ole) "float_le" lhs rhs

    let eq : (float, _) m -> (float, _) m -> (bool, unknown) m =
     fun lhs rhs ->
      bool_binop (Llvm.build_fcmp Llvm.Fcmp.Oeq) "float_eq" lhs rhs
  end

  let ( && ) lhs rhs = bool_binop Llvm.build_and "bool_and" lhs rhs

  let ( || ) lhs rhs = bool_binop Llvm.build_or "bool_or" lhs rhs

  let array : ('a, const) m array -> ('a array, const) m =
   fun elements ->
    let open LLVM_state in
    if Array.length elements = 0 then
      invalid_arg "array: array literals must have nonzero length" ;
    let rec loop elements acc =
      match elements with
      | [] -> return (List.rev acc)
      | hd :: tl ->
          let* hd in
          loop tl (hd :: acc)
    in
    let* elements = loop (Array.to_list elements) [] in
    let elements = Array.of_list elements in
    let elt_ty = typeof elements.(0) in
    let* llvm_elt_ty = LLVM_type.of_type elt_ty in
    let array = Llvm.const_array llvm_elt_ty (Array.map llval elements) in
    llreturn array (Type_system.vec ~static_size:(Array.length elements) elt_ty)

  let struct_ : type intro. (intro, _, _ vec, _ vec, _) record -> _ -> intro =
    fun (type res u) (record : (intro, _, res vec, res vec, u) record) _conv ->
     let open LLVM_state in
     let rec loop :
         type intro elim acc.
         (intro, elim, acc vec, res vec, u) Type_system.record ->
         _ ->
         _ ->
         intro =
      fun (descr : (intro, _, _, res vec, u) Type_system.record)
          acc
          named_struct_id_opt ->
       match descr with
       | Record_empty ->
           (let* context in
            let* acc in
            let vs = List.rev acc in
            let strct =
              match named_struct_id_opt with
              | None -> Llvm.const_struct context (Array.of_list vs)
              | Some id -> (
                  match Hashtbl.find_opt LLVM_type.struct_table id with
                  | None -> assert false
                  | Some ty -> Llvm.const_named_struct ty (Array.of_list vs))
            in
            llreturn strct (Type_system.seal record)
             : intro)
       | Record_field (_, rest) ->
           (fun (arg : _ m) ->
              loop
                rest
                (let* arg in
                 let* acc in
                 return (llval arg :: acc))
                named_struct_id_opt
             : intro)
       | Record_fix (id, f) -> loop (f (seal descr)) acc (Some id)
     in
     loop record (return []) None

  let projs : type elim res u. (_, elim, _, res, u) record -> _ -> elim =
   fun record _conv ->
    let open LLVM_state in
    let rec loop :
        type intro elim acc.
        (intro, elim, acc, res, u) Type_system.record -> int -> elim =
     fun (descr : (intro, elim, acc, res, u) Type_system.record) index ->
      match descr with
      | Record_empty -> (() : elim)
      | Record_field (field, rest) ->
          let proj =
            { proj =
                (fun (type c) (record : (u ref, c) m) ->
                  let* builder in
                  let* record in
                  let elt_ptr =
                    Llvm.build_struct_gep
                      (llval record)
                      index
                      ("fieldaddr_" ^ string_of_int index)
                      builder
                  in
                  llreturn elt_ptr (Type_system.ptr field.ty))
            }
          in
          let elims = loop rest (index + 1) in
          ((elims, proj) : elim)
      | Record_fix (_id, f) -> loop (f (seal descr)) index
    in
    loop record 0

  let seq (m : (unit, _) m) (f : unit -> ('b, _) m) : ('b, unknown) m =
    let open LLVM_state in
    let* _m = m in
    f ()

  let ( let* ) (m : ('a, _) m) (f : ('a, _) m -> ('b, _) m) : ('b, unknown) m =
    let open LLVM_state in
    let* m in
    f (return m)

  let store (type a) (ptr : (a ref, not_const) m) (v : (a, _) m) :
      (unit, unknown) m =
    let open LLVM_state in
    let* builder in
    let* ptr in
    let* v in
    let _ = Llvm.build_store (llval v) (llval ptr) builder in
    unit_unknown

  let load (type a) (ptr : (a ref, 'c) m) : (a, 'c) m =
    let open LLVM_state in
    let* builder in
    let* ptr in
    match typeof ptr with
    | TPtr typ -> llreturn (Llvm.build_load (llval ptr) "load_tmp" builder) typ
    | TNum _ | TRecord _ -> assert false

  let get (type a) (arr : (a array, 'c) m) (i : (int64, _) m) : (a, 'c) m =
    let open LLVM_state in
    let* builder in
    let* arr in
    let* i in
    match (typeof arr, typeof i) with
    | (TVec (sz_opt, typ), TNum Int64_num) ->
        let addr =
          match sz_opt with
          | None -> Llvm.build_gep (llval arr) [| llval i |] "get_gep" builder
          | Some _ ->
              Llvm.build_in_bounds_gep
                (llval arr)
                [| llval i |]
                "get_gep_inbounds"
                builder
        in
        llreturn (Llvm.build_load addr "get_load_tmp" builder) typ
    | _ -> assert false

  let set (type a) (arr : (a array, not_const) m) (i : (int64, _) m)
      (e : (a, _) m) : (unit, unknown) m =
    let open LLVM_state in
    let* builder in
    let* arr in
    let* i in
    let* e in
    let addr =
      match typeof arr with
      | TVec (None, _) ->
          Llvm.build_gep (llval arr) [| llval i |] "get_gep" builder
      | TVec (Some _, _) ->
          Llvm.build_in_bounds_gep (llval arr) [| llval i |] "get_gep" builder
      | TNum _ | TRecord _ -> assert false
    in
    let _ = Llvm.build_store (llval e) addr builder in
    unit_unknown

  let cond (type t) (cond : (bool, _) m) (dispatch : bool -> (t, _) m) =
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
    let phi_block = Llvm.append_block context "ifjoin" enclosing_func in
    Llvm.position_at_end phi_block builder ;
    let incoming = List.[(llval bt, trueblock'); (llval bf, falseblock')] in
    let phi = Llvm.build_phi incoming "phitmp" builder in
    (* move the instruction builder back at he the end of [end_of_cond_pos],
           emit appropriate jump. *)
    Llvm.position_at_end end_of_cond_pos builder ;
    ignore (Llvm.build_cond_br (llval cond) trueblock falseblock builder) ;
    (* insert jumps from end of the 'true branch' block to the merge node. *)
    Llvm.position_at_end trueblock' builder ;
    ignore (Llvm.build_br phi_block builder) ;
    (* insert jumps from end of the 'true branch' block to the merge node. *)
    Llvm.position_at_end falseblock' builder ;
    ignore (Llvm.build_br phi_block builder) ;
    (* Move inserter at end of join block. *)
    Llvm.position_at_end phi_block builder ;
    llreturn phi (typeof bf)

  let for_ :
      init:(int64, _) m ->
      pred:((int64, _) m -> (bool, _) m) ->
      step:((int64, _) m -> (int64, _) m) ->
      ((int64, _) m -> (unit, _) m) ->
      (unit, _) m =
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

    let* phi_ty = LLVM_type.of_type Type_system.int64 in
    let phi = Llvm.build_empty_phi phi_ty "for_phi" builder in
    Llvm.add_incoming (llval init, last_init_block) phi ;
    let phi_expr = llreturn phi Type_system.int64 in
    let* cond = pred phi_expr in
    let _ = Llvm.build_cond_br (llval cond) for_body for_exit builder in

    Llvm.position_at_end for_body builder ;
    seq (body phi_expr) @@ fun () ->
    let* next = step phi_expr in
    let _ = Llvm.build_br for_entry builder in

    Llvm.add_incoming (llval next, Llvm.insertion_block builder) phi ;
    Llvm.position_at_end for_exit builder ;
    unit_unknown

  let switch_i64 :
      (int64, _) m ->
      cases:(int64 * (unit -> ('a, _) m)) array ->
      default:(unit -> ('a, _) m) ->
      ('a, _) m =
   fun expr ~cases ~default ->
    let open LLVM_state in
    let* builder in
    let* context in

    let current_block = Llvm.insertion_block builder in
    let enclosing_func = Llvm.block_parent current_block in

    let non_default_cases = Array.length cases in

    let switch_block =
      Llvm.append_block context "switch_entry" enclosing_func
    in
    (* Add unconditional jump from [current_block] inherited from context to [switch_block] *)
    let _ = Llvm.build_br switch_block builder in

    let cases =
      Array.mapi
        (fun i (const, case) ->
          let block =
            Llvm.append_block
              context
              (Printf.sprintf "switch_case_%Ld_%d" const i)
              enclosing_func
          in
          (const, case, block))
        cases
    in
    let default_block =
      Llvm.append_block context "switch_case_default" enclosing_func
    in
    let phi_block = Llvm.append_block context "switch_phi" enclosing_func in

    Llvm.position_at_end switch_block builder ;
    (* Generate code for expr and switch, set default branch *)
    let* expr in
    let switch =
      Llvm.build_switch (llval expr) default_block non_default_cases builder
    in

    (* Build default block *)
    Llvm.position_at_end default_block builder ;
    let* default = default () in
    let _ = Llvm.build_br phi_block builder in

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
        let _ = Llvm.build_br phi_block builder in
        loop (i + 1) ((llval case, block) :: acc)
    in

    let* non_default_cases = loop 0 [] in
    let all_cases = (llval default, default_block) :: non_default_cases in
    Llvm.position_at_end phi_block builder ;
    let result = Llvm.build_phi all_cases "switch_phi" builder in
    llreturn result (typeof default)

  type init =
    | Scalar_init : { init : ('a, _) m; ptr : Llvm.llvalue } -> init
    | Array_init :
        { init : ('a, _) m; arr : ('a array, not_const) m; size : Llvm.llvalue }
        -> init
  (* | Array_init_static :
   *     { ty : 'a Type_system.typ;
   *       f : int -> 'a m;
   *       arr : 'a array m;
   *       size : int
   *     }
   *     -> init *)

  let rec alloca :
      type a s ret.
      (a, s -> (ret, unknown) m) Stack_frame.t ->
      init list ->
      a ->
      s ->
      (ret, unknown) m =
    let open LLVM_state in
    fun (type a s ret)
        (frame : (a, s -> (ret, unknown) m) Stack_frame.t)
        initializers
        (k : a)
        (s : s) ->
      match frame with
      | Empty ->
          let rec initialize init =
            match init with
            | [] -> k s
            | Scalar_init { init; ptr } :: rest ->
                let* builder in
                let* init in
                let store = Llvm.build_store (llval init) ptr builder in
                Format.printf
                  "init: %s with type %a@."
                  (Llvm.string_of_llvalue (llval init))
                  Type_system.pp_typ
                  (typeof init) ;
                Format.printf "store: %s@." (Llvm.string_of_llvalue store) ;
                initialize rest
            | Array_init { init; arr; size } :: rest ->
                let size = llreturn size Type_system.int64 in
                let* _ =
                  for_
                    ~init:(I64.v 0L)
                    ~pred:(fun i -> I64.lt i size)
                    ~step:(fun i -> I64.add i (I64.v 1L))
                    (fun i -> set arr i init)
                in
                initialize rest
            (* | Array_init_static { ty; f; arr; size } :: rest ->
             *     let rec loop i acc =
             *       if i = size then return (Array.of_list (List.rev acc))
             *       else
             *         let* elt = f i in
             *         loop (i + 1) (llval elt :: acc)
             *     in
             *     let* builder in
             *     let* arr in
             *     let* elts = loop 0 [] in
             *     let* ty = LLVM_type.of_type ty in
             *     let init = Llvm.const_array ty elts in
             *     let _ = Llvm.build_store init (llval arr) builder in
             *     initialize rest *)
          in
          initialize initializers
      | Cons (Single (typ, init), rest) ->
          let* builder in
          let* lltyp = LLVM_type.of_type typ in
          let llalloca = Llvm.build_alloca lltyp "loc" builder in
          let expr = llreturn llalloca (ptr typ) in
          alloca
            rest
            (Scalar_init { init; ptr = llalloca } :: initializers)
            (k expr)
            s
      | Cons (Array (typ, init, size), rest) ->
          let* builder in
          let* lltyp = LLVM_type.of_type typ in
          let* size in
          let llalloca =
            Llvm.build_array_alloca
              lltyp
              (llval size)
              "alloca_array_tmp"
              builder
          in
          let arr = llreturn llalloca (vec typ) in
          let init = Array_init { init; arr; size = llval size } in
          alloca rest (init :: initializers) (k arr) s
  (* | Cons (Array_static (ty, f, static_size), rest) ->
   *     let* builder in
   *     let* lltyp = LLVM_type.of_type ty in
   *     let* size = I64.v (Int64.of_int static_size) in
   *     let llalloca =
   *       Llvm.build_array_alloca
   *         lltyp
   *         (llval size)
   *         "alloca_array_tmp"
   *         builder
   *     in
   *     let arr = llreturn llalloca (vec ~static_size ty) in
   *     let init = Array_init_static { ty; f; arr; size = static_size } in
   *     alloca rest (init :: initializers) (k arr) s *)

  let rec prototype :
      type s ret.
      (s, (ret, unknown) m) Prototype.t ->
      Llvm.lltype list ->
      Llvm.lltype LLVM_state.t =
    fun (type s ret) (proto : (s, (ret, unknown) m) Prototype.t) acc ->
     let open LLVM_state in
     match proto with
     | Prototype.Returning ty ->
         let* retty = LLVM_type.of_type ty in
         return (Llvm.function_type retty (Array.of_list (List.rev acc)))
     | Prototype.Arrow (ty, rest) ->
         let* llty = LLVM_type.of_type ty in
         prototype rest (llty :: acc)

  let rec args_to_tuple :
      type s ret. (s, ret) Prototype.t -> Llvm.llvalue list -> s option =
   fun proto args ->
    let open LLVM_state in
    match (proto, args) with
    | (Prototype.Returning _, []) -> Some ()
    | (Prototype.Arrow (typ, rest), arg :: args) ->
        Option.map
          (fun rest -> (llreturn arg typ, rest))
          (args_to_tuple rest args)
    | _ -> None

  let fundecl :
      name:string ->
      signature:('s, ('ret, unknown) m) Prototype.t ->
      local:('b, 's -> ('ret, unknown) m) Stack_frame.t ->
      body:'b ->
      ('s, ('ret, unknown) m) fundecl k =
   fun ~name ~signature ~local ~body ->
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
        let* res = alloca local [] body args in
        let _ = Llvm.build_ret (llval res) builder in
        let fundecl = { name; signature; fptr = fn } in
        if not (Llvm_analysis.verify_function fn) then (
          let* _ = LLVM_state.dump_module in
          Llvm_analysis.assert_valid_function fn ;
          failwith "Aborting")
        else return fundecl

  let call : type s ret. (s, ret) fundecl -> s Prototype.args -> ret =
   fun f args ->
    let open LLVM_state in
    let rec loop :
        type s ret.
        (s, ret) Prototype.t -> s Prototype.args -> Llvm.llvalue list t -> ret =
     fun proto args acc ->
      match (proto, args) with
      | (Prototype.Returning retty, Prototype.[]) ->
          let* builder in
          let* acc in
          let call =
            Llvm.build_call f.fptr (Array.of_list (List.rev acc)) "call" builder
          in
          llreturn call retty
      | (Prototype.Arrow (_ty, proto), Prototype.(expr :: args)) ->
          let* expr in
          loop
            proto
            args
            (let* acc in
             return (llval expr :: acc))
    in
    loop f.signature args (return [])
end
