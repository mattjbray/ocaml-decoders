(** Turn S-expressions into Ocaml values via Sexplib. *)

(** Following the convention of [Sexplib0.Sexp_conv.hashtbl_of_sexp], we
    consider an S-expression to be an "object" if it is a list of two-element
    lists. For example:

        ((field1 value2) (field2 value2))

    Like YAML, fields of an object are not necessarily atoms. To handle these,
    look for the primed combinators (e.g. [keys']).
*)

include Decoders.Decode.S with type value = Sexplib0.Sexp.t
