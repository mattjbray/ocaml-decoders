(** {2 Yojson implementation} *)

open Decoders

module Json_decodeable : Decode.Decodeable with type value = Yojson.Raw.json = struct
  type value = Yojson.Raw.json
  let pp fmt json = Format.fprintf fmt "@[%s@]" (Yojson.Raw.pretty_to_string json)

  let of_string : string -> (value, string) result =
    fun string ->
      try Ok (Yojson.Raw.from_string string) with
      | Yojson.Json_error msg -> Error (msg)

  let of_file file =
    try Ok (Yojson.Raw.from_file file) with
    | e -> Error (Printexc.to_string e)

  let get_string = function
    | `Stringlit s ->
      (* Stringlits are wrapped in double-quotes. *)
      Some (String.sub s 1 (String.length s - 2))
    | _ -> None

  let get_int = function
    | `Intlit value -> Some (int_of_string value)
    | _ -> None

  let get_float = function
    | `Floatlit value -> Some (float_of_string value)
    | `Intlit value -> Some (float_of_string value)
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
    | `Assoc assoc -> Some (List.map (fun (key, value) -> (`Stringlit (Printf.sprintf "%S" key), value)) assoc)
    | _ -> None

  let to_list values = `List values
end

module Decode = struct
  include Decode.Make(Json_decodeable)

  (* Yojson.Raw specific decoders *)

  let stringlit : string decoder =
    { run =
        function
        | `Stringlit value -> Ok value
        | json -> (fail "Expected a string").run json
    }

  let intlit : string decoder =
    { run =
        function
        | `Intlit value -> Ok value
        | json -> (fail "Expected an int").run json
    }

  let floatlit : string decoder =
    { run =
        function
        | `Floatlit value -> Ok value
        | `Intlit value -> Ok value
        | json -> (fail "Expected a float").run json
    }
end


module Json_encodeable = struct
  type value = Yojson.Raw.json

  let to_string json = Yojson.Raw.to_string json

  let of_string x = `Stringlit (Printf.sprintf "%S" x)
  let of_int x = `Intlit (string_of_int x)
  let of_float x = `Floatlit (string_of_float x)
  let of_bool x = `Bool x
  let null = `Null

  let of_list xs = `List xs
  let of_key_value_pairs xs =
    `Assoc
      (xs
       |> Decoders_util.My_list.filter_map (fun (k, v) ->
           Json_decodeable.get_string k
           |> Decoders_util.My_opt.map (fun k -> (k, v))
         ))
end

module Encode = struct
  include Encode.Make(Json_encodeable)

  let stringlit x = `Stringlit x
  let intlit x = `Intlit x
  let floatlit x = `Floatlit x
end
