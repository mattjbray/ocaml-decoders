(** Turn JSON values into Ocaml values. *)

module Decode : sig
  include Decoders.Decode.S with type value = Js.Json.t
end

module Encode : sig
  include Decoders.Encode.S with type value = Js.Json.t
end
