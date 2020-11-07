(** Turn JSON values into Ocaml values. *)

module Decode : sig
  include Decoders.Decode.S with type value = Js.Json.t

  (** {2 bs-specific decoders}*)

  val array : 'a decoder -> 'a array decoder
end

module Encode : sig
  include Decoders.Encode.S with type value = Js.Json.t

  (** {2 bs-specific encoders}*)

  val array : 'a encoder -> 'a array encoder
end
