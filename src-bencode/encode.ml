module Bencode_encodeable = struct
  type value = Bencode.t

  let to_string value = Bencode.encode_to_string value

  let of_string x = Bencode.String x
  let of_int x = Bencode.Integer x
  let of_float x = Bencode.String (string_of_float x)
  let of_bool x = Bencode.Integer (if x then 1 else 0)
  let null = Bencode.Integer 0

  let of_list xs = Bencode.List xs
  let of_key_value_pairs xs =
    let xs =
      CCList.filter_map
        (function 
          | Bencode.String s, v -> Some (s,v)
          | _ -> None)
        xs
    in
    Bencode.Dict xs
end

include Decoders.Encode.Make(Bencode_encodeable)
