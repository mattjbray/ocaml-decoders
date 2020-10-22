include Decoders.Decode.S with type value = Msgpck.t

val bytes : string decoder
val int32 : int32 decoder
val int64 : int64 decoder
val uint32 : int32 decoder
val uint64 : int64 decoder
val ext : (int * string) decoder
