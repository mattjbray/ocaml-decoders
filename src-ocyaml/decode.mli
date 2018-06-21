(** Turn Yaml values into Ocaml values. *)

include Decoders.Decode.S with type value = Ocyaml.yaml
