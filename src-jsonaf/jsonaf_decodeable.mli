open Decoders

type value = Jsonaf.t

include Decode.Decodeable with type value := value

val get_number : value -> string option
