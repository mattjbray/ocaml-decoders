(** {2 Yojson implementation} *)

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
end

include Decode.Make(Json_decodeable)

(* Yojson.Raw specific decoders *)

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
