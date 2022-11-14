open Decoders

module Decode = struct
  include Decode.Make (Jsonaf_decodeable)

  let number : string decoder =
    { Decoder.dec =
        (fun t ->
          match Jsonaf_decodeable.get_number t with
          | Some value ->
              Ok value
          | None ->
              Error (Decoders.Error.make ~context:t "not a number") )
    }
end

module Encode = struct
  include Encode.Make (Jsonaf_encodeable)

  let number n = Jsonaf_encodeable.of_number n
end
