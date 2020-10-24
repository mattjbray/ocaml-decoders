(** Turn JSON values into Ocaml values. *)

module Decode : Decoders.Decode.S with type value = Js.Json.t

module Encode : Decoders.Encode.S with type value = Js.Json.t
