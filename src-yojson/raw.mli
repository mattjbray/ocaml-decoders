(** Turn JSON values into Ocaml values. *)

module Decode : sig
  include Decoders.Decode.S with type value = Yojson.Raw.t

  (** {2 Yojson.Raw-specific decoders}*)

  val stringlit : string decoder

  val intlit : string decoder

  val floatlit : string decoder
end

module Encode : sig
  include Decoders.Encode.S with type value = Yojson.Raw.t

  (** {2 Yojson.Raw-specific encoders}*)

  val stringlit : string encoder

  val intlit : string encoder

  val floatlit : string encoder
end
