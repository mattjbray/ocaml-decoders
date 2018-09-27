(** Turn JSON values into Ocaml values via Jsonm. *)

include Decoders.Decode.S with type value = Ezjsonm.value
