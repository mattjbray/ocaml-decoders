(** Turn JSON values into Ocaml values. *)

include Decode.S with type t = Yojson.Raw.json

(** {1 JSON-specific decoders} *)

val json_of_file : string -> (t, error) result

(** {1 Yojson.Raw-specific decoders}*)

val intlit : string decoder
val floatlit : string decoder
