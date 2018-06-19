(** Turn Yaml values into Ocaml values. *)

include Decode.S with type value = Ocyaml.yaml

(** {1 Yaml-specific decoders}

    Use these decoders when working with objects whose keys are not strings.
*)

(** Decode a Yaml object into a list of key-value pairs. *)
val key_value_pairs_yaml : 'k decoder -> 'v decoder -> ('k * 'v) list decoder

(** Decode a Yaml object into a list of values, where the value decoder
    depends on the output of the key decoder. *)
val key_value_pairs_seq_yaml : 'k decoder -> ('k -> 'v decoder) -> 'v list decoder
