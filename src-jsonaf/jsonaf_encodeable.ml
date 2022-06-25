type value = Jsonaf.t

let to_string json = Jsonaf.to_string json

let of_string s = `String s

let of_int i = `Number (string_of_int i)

let of_number n = `Number n

let of_float f =
  let s = string_of_float f in
  let len = String.length s in
  if String.ends_with ~suffix:"." s
  then `Number (String.sub s 0 (len - 1))
  else `Number s


let of_bool x = if x then `True else `False

let null = `Null

let of_list xs = `Array xs

let of_key_value_pairs xs =
  let string_keyed (k, v) =
    match k with `String k -> Some (k, v) | _ -> None
  in
  let pairs = List.filter_map string_keyed xs in
  `Object pairs
