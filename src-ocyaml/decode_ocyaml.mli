(** Turn Yaml values into Ocaml values. *)

include Decode.S with type value = Ocyaml.yaml
