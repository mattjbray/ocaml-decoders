(** Turn JSON values into Ocaml values. *)

module Decode : Decoders.Decode.S with type value = Yojson.Basic.t

module Encode : Decoders.Encode.S with type value = Yojson.Basic.t
