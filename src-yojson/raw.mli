(** Turn JSON values into Ocaml values. *)

module Decode : sig
  include Decoders.Decode.S with type value = Yojson.Raw.json

  (** {1 Yojson.Raw-specific decoders}*)

  val intlit : string decoder
  val floatlit : string decoder
end

module Encode : sig
  include Decoders.Encode.S with type value = Yojson.Raw.json

  (** {1 Yojson.Raw-specific encoders}*)

  val intlit : string encoder
  val floatlit : string encoder
end
