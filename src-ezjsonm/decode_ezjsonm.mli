(** Turn JSON values into Ocaml values via Jsonm. *)

include Decode.S with type t = Ezjsonm.value
