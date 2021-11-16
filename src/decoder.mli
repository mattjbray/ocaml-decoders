(** An [('i, 'o, 'e) t] is a decoder that

    - consumes a value of type ['i]
    - producing a value of type ['o]
    - or an error of type ['e]
 *)
type ('i, 'o, 'e) t = 'i -> ('o, 'e) result

val pure : 'o -> ('i, 'o, 'e) t
(** [pure x] always succeeds with [x] *)

val fail : 'e -> ('i, 'o, 'e) t
(** [fail e] always fails with [e] *)

val of_result : ('o, 'e) Decoders_util.My_result.t -> ('i, 'o, 'e) t

val bind : ('a -> ('i, 'b, 'e) t) -> ('i, 'a, 'e) t -> ('i, 'b, 'e) t

val map : ('a -> 'b) -> ('i, 'a, 'e) t -> ('i, 'b, 'e) t

val map_err : ('e1 -> 'e2) -> ('i, 'o, 'e1) t -> ('i, 'o, 'e2) t

val apply : ('i, 'a -> 'b, 'e) t -> ('i, 'a, 'e) t -> ('i, 'b, 'e) t

module Infix : sig
  val ( >>= ) : ('i, 'a, 'e) t -> ('a -> ('i, 'b, 'e) t) -> ('i, 'b, 'e) t

  val ( >|= ) : ('i, 'a, 'e) t -> ('a -> 'b) -> ('i, 'b, 'e) t

  val ( <*> ) : ('i, 'a -> 'b, 'e) t -> ('i, 'a, 'e) t -> ('i, 'b, 'e) t

  include Shims_let_ops_.S with type ('i, 'o, 'e) t_let = ('i, 'o, 'e) t
end

val fix : (('i, 'a, 'e) t -> ('i, 'a, 'e) t) -> ('i, 'a, 'e) t

val value : ('i, 'i, 'e) t

val maybe : ('i, 'a, 'e) t -> ('i, 'a option, 'e) t

val one_of :
  combine_errors:('e list -> 'e) -> ('i, 'o, 'e) t list -> ('i, 'o, 'e) t

val pick :
     combine_errors:('e list -> 'e)
  -> ('i, ('i, 'o, 'e) t, 'e) t list
  -> ('i, 'o, 'e) t

val of_to_opt : ('i -> 'o option) -> ('i -> ('o, 'e) result) -> ('i, 'o, 'e) t
