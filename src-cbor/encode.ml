module Cbor_encodeable = struct
  type value = CBOR.Simple.t

  let to_string value = CBOR.Simple.encode value

  let of_string x = `Text x
  let of_int x = `Int x
  let of_float x = `Float x
  let of_bool x = `Bool x
  let null = `Null

  let of_list xs = `Array xs
  let of_key_value_pairs xs =
    `Map xs
end

include Decoders.Encode.Make(Cbor_encodeable)

let undefined : unit encoder =
  fun _ -> `Undefined

let simple : int encoder =
  fun i -> `Simple i

let bytes : string encoder =
  fun b -> `Bytes b
