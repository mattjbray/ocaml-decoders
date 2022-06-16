(** An [('i, 'o) t] is a decoder that

    - consumes a value of type ['i]
    - produces a value of type ['o] or an error of type ['i Error.t]
 *)
type ('i, 'o) t = 'i -> ('o, 'i Error.t) result

val pure : 'o -> ('i, 'o) t
(** [pure x] always succeeds with [x] *)

val fail : string -> ('i, 'o) t
(** [fail msg] always fails with [msg], capturing the error context from 'i *)

val fail_with : 'i Error.t -> ('i, 'o) t
(** [fail_with e] always fails with [e] *)

val of_result : ('o, 'i Error.t) Decoders_util.My_result.t -> ('i, 'o) t

val bind : ('a -> ('i, 'b) t) -> ('i, 'a) t -> ('i, 'b) t

val map : ('a -> 'b) -> ('i, 'a) t -> ('i, 'b) t

val map_err : ('i Error.t -> 'i Error.t) -> ('i, 'o) t -> ('i, 'o) t

val apply : ('i, 'a -> 'b) t -> ('i, 'a) t -> ('i, 'b) t

module Infix : sig
  val ( >>= ) : ('i, 'a) t -> ('a -> ('i, 'b) t) -> ('i, 'b) t

  val ( >|= ) : ('i, 'a) t -> ('a -> 'b) -> ('i, 'b) t

  val ( <*> ) : ('i, 'a -> 'b) t -> ('i, 'a) t -> ('i, 'b) t

  include Shims_let_ops_.S with type ('i, 'o) t_let = ('i, 'o) t
end

val fix : (('i, 'a) t -> ('i, 'a) t) -> ('i, 'a) t

val value : ('i, 'i) t

val maybe : ('i, 'a) t -> ('i, 'a option) t

val one_of : ('i, 'o) t list -> ('i, 'o) t

val pick : ('i, ('i, 'o) t) t list -> ('i, 'o) t

val of_to_opt :
  ('i -> 'o option) -> ('i -> ('o, 'i Error.t) result) -> ('i, 'o) t

val decode_sub : 'a -> ('a, 'b) t -> ('a, 'b) t
