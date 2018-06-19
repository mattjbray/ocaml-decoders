(** Turn JSON values into Ocaml values. *)

include Decode.S with type value = Yojson.Raw.json

(** {1 JSON-specific decoders} *)

val json_of_file : string -> (value, error) result

(** {1 Yojson.Raw-specific decoders}*)

val intlit : string decoder
val floatlit : string decoder
