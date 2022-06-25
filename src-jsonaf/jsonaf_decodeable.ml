type value = Jsonaf.t

let pp fmt json = Format.fprintf fmt "@[%s@]" (Jsonaf.to_string_hum json)

let of_string : string -> (value, string) Result.t =
 fun string ->
  match Jsonaf.parse string with
  | Ok json ->
      Ok json
  | Error e ->
      Error (Base.Error.to_string_hum e)


let of_file _ = failwith "Json_decodeable.of_file: not implemented"

let get_string : value -> string option = function
  | `String value ->
      Some value
  | _ ->
      None


let get_number : value -> string option = function
  | `Number value ->
      Some value
  | _ ->
      None


let get_int : value -> int option = function
  | `Number value ->
      int_of_string_opt value
  | _ ->
      None


let get_float : value -> float option = function
  | `Number value ->
      float_of_string_opt value
  | _ ->
      None


let get_bool : value -> bool option = function
  | `True ->
      Some true
  | `False ->
      Some false
  | _ ->
      None


let get_null : value -> unit option = function `Null -> Some () | _ -> None

let get_list : value -> value list option = function
  | `Array values ->
      Some values
  | _ ->
      None


let get_key_value_pairs : value -> (value * value) list option = function
  | `Object pairs ->
      Some (List.map (fun (k, v) -> (`String k, v)) pairs)
  | _ ->
      None


let to_list values = `Array values
