open Decoders

type value = Jsonaf.t

include Encode.Encodeable with type value := value

val of_number : string -> value
