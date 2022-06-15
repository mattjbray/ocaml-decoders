(** An ['a t] is an error with a contextual value of type ['a] *)
type 'a t

val make : ?context:'a -> string -> 'a t

val tag : string -> 'a t -> 'a t

val group : 'a t list -> 'a t

val tag_group : string -> 'a t list -> 'a t

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val to_string : (Format.formatter -> 'a -> unit) -> 'a t -> string

val map_tag : (string -> string) -> 'a t -> 'a t
