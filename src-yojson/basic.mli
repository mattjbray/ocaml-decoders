(** Turn JSON values into Ocaml values. *)

include Decode.S with type value = Yojson.Basic.json

(** {1 JSON-specific decoders} *)

val json_of_file : string -> (value, error) result
