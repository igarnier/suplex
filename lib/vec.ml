(** The type of heterogenous vectors. *)
type _ t = [] : unit t | ( :: ) : 'a * 'b t -> ('a * 'b) t
