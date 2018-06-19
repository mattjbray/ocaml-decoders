(** Turn JSON values into Ocaml values. *)

include Decode.S with type value = Yojson.Basic.json
