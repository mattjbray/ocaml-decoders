(** Turn S-expressions into Ocaml values via Sexplib. *)

include Decoders.Decode.S with type value = Sexplib0.Sexp.t
