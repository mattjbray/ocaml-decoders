(** Turn JSON values into Ocaml values. *)

module Make
    (Toml : Otoml.Base.TomlImplementation
              with type toml_integer = int
               and type toml_float = float) : sig
  module Decode : Decoders.Decode.S with type value = Toml.t

  module Encode : Decoders.Encode.S with type value = Toml.t
end

module Decode : Decoders.Decode.S with type value = Otoml.t

module Encode : Decoders.Encode.S with type value = Otoml.t
