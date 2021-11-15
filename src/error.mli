(** An ['i t] is an error with a contextual value of type ['i] *)
type 'i t

val make : ?context:'a -> string -> 'a t

val tag : string -> 'a t -> 'a t

val group : 'a t list -> 'a t

val tag_group : string -> 'a t list -> 'a t

val pp :
  pp_i:(Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val map_tag : (string -> string) -> 'i t -> 'i t
