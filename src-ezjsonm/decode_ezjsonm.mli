(** Turn JSON values into Ocaml values via Jsonm. *)

include Decode.S with type value = Ezjsonm.value

val of_file : string -> (value, error) result
val decode_file : 'a decoder -> string -> ('a, error) result
