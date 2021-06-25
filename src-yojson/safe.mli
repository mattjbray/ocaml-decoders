(** Turn JSON values into Ocaml values. *)

module Decode : Decoders.Decode.S with type value = Yojson.Safe.t

module Encode : Decoders.Encode.S with type value = Yojson.Safe.t
