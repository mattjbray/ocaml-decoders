module type S = Sig.S

(** {2 Creating a Decoder implementation}

    The following is useful only if you are creating a new Decoder implementation.
*)

(** Signature of things that can be decoded. *)
module type Decodeable = Sig.Decodeable

(** Derive decoders for a [Decodeable.value]. *)
module Make (M : Decodeable) :
  Sig.S with type value = M.value and type 'a decoder = (M.value, 'a) Decoder.t
