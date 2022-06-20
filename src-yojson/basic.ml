(** {2 Yojson implementation} *)

open Decoders

module Json_decodeable : Decode.Decodeable with type value = Yojson.Basic.t =
struct
  type value = Yojson.Basic.t

  let pp fmt json =
    Format.fprintf fmt "@[%s@]" (Yojson.Basic.pretty_to_string json)


  let of_string : string -> (value, string) result =
   fun string ->
    try Ok (Yojson.Basic.from_string string) with
    | Yojson.Json_error msg ->
        Error msg


  let of_file file =
    try Ok (Yojson.Basic.from_file file) with
    | e ->
        Error (Printexc.to_string e)


  let get_string = function `String value -> Some value | _ -> None

  let get_int = function `Int value -> Some value | _ -> None

  let get_float = function
    | `Float value ->
        Some value
    | `Int value ->
        Some (float_of_int value)
    | _ ->
        None


  let get_bool = function `Bool value -> Some value | _ -> None

  let get_null = function `Null -> Some () | _ -> None

  let get_list : value -> value list option = function
    | `List l ->
        Some l
    | _ ->
        None


  let get_key_value_pairs : value -> (value * value) list option = function
    | `Assoc assoc ->
        Some (List.map (fun (key, value) -> (`String key, value)) assoc)
    | _ ->
        None


  let to_list values = `List values
end

module Decode = Decode.Make (Json_decodeable)

module Json_encodeable = struct
  type value = Yojson.Basic.t

  let to_string json = Yojson.Basic.to_string json

  let of_string x = `String x

  let of_int x = `Int x

  let of_float x = `Float x

  let of_bool x = `Bool x

  let null = `Null

  let of_list xs = `List xs

  let of_key_value_pairs xs =
    `Assoc
      ( xs
      |> Util.My_list.filter_map (fun (k, v) ->
             match k with `String k -> Some (k, v) | _ -> None ) )
end

module Encode = Encode.Make (Json_encodeable)
