include Decoders.Decode.S with type value = Msgpck.t

val string_strict : string decoder
(** Only accepts [String], not [Bytes]. The string should be valid UTF8
    per the spec. *)

val bytes : string decoder
(** Raw data only. *)

val int32 : int32 decoder

val int64 : int64 decoder

val uint32 : int32 decoder

val uint64 : int64 decoder

val ext : (int * string) decoder
