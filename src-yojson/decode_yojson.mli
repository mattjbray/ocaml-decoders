(** Turn JSON values into Ocaml values. *)

include Decode.S with type t = Yojson.Basic.json

(** {1 JSON-specific decoders} *)

val json_of_file : string -> (t, error) result
