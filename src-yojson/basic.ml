(** {2 Yojson implementation} *)

module Json_decodeable : Decode.Decodeable with type value = Yojson.Basic.json = struct
  type value = Yojson.Basic.json
  let pp fmt json = Format.fprintf fmt "@[%s@]" (Yojson.Basic.pretty_to_string json)

  let get_string = function
    | `String value -> Some value
    | _ -> None

  let get_int = function
    | `Int value -> Some value
    | _ -> None

  let get_float = function
    | `Float value -> Some value
    | `Int value -> Some (float_of_int value)
    | _ -> None

  let get_bool = function
    | `Bool value -> Some value
    | _ -> None

  let get_null = function
    | `Null -> Some ()
    | _ -> None

  let get_list : value -> value list option = function
    | `List l -> Some l
    | _ -> None

  let get_key_value_pairs : value -> (value * value) list option = function
    | `Assoc assoc -> Some (List.map (fun (key, value) -> (`String key, value)) assoc)
    | _ -> None

  let get_field key = function
    | `Assoc assoc -> CCList.assoc_opt ~eq:(=) key assoc
    | _ -> None

  let get_single_field = function
    | `Assoc [(key, value)] -> Some (key, value)
    | _ -> None

  let of_string : string -> (value, string) result =
    fun string ->
      try Ok (Yojson.Basic.from_string string) with
      | Yojson.Json_error msg -> Error msg
end


include Decode.Make(Json_decodeable)

open Yojson.Basic

let json_of_file file =
  try Ok (from_file file) with
  | e -> Error (Decoder_error (Printexc.to_string e, None))
