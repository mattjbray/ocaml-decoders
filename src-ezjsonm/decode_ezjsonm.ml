module Ezjsonm_decodeable : Decode.Decodeable with type t = Ezjsonm.value = struct
  type t = Ezjsonm.value

  let pp_t fmt t =
    match t with
    | `Null -> Format.fprintf fmt "@[null@]"
    | `Bool bool -> Format.fprintf fmt "@[%a@]" Format.pp_print_bool bool
    | `Float float -> Format.fprintf fmt "@[%a@]" Format.pp_print_float float
    | `String string -> Format.fprintf fmt "@[%S@]" string
    | (`A _ | `O _) as t -> Format.fprintf fmt "@[%s@]" Ezjsonm.(to_string t)

  let pp fmt t = Format.fprintf fmt "@[%a@]" pp_t t

  let of_string (input : string) : (t, string) result =
    try Ok (Ezjsonm.from_string input) with
    | Ezjsonm.Parse_error (json, msg) -> Error msg

  let get_string = function
    | `String str -> Some str
    | _ -> None

  let get_int = function
    | `Float float ->
      (* TODO: fail if not an int? *)
      Some (int_of_float float)
    | _ -> None

  let get_float = function
    | `Float float -> Some float
    | _ -> None

  let get_null = function
    | `Null -> Some ()
    | _ -> None

  let get_bool = function
    | `Bool bool -> Some bool
    | _ -> None

  let get_list = function
    | `A a -> Some a
    | _ -> None

  let get_single_field = function
    | `O [(key, value)] -> Some (key, value)
    | _ -> None

  let get_field key = function
    | `O assoc -> List.assoc_opt key assoc
    | _ -> None
end

include Decode.Make(Ezjsonm_decodeable)

let keys : string list decoder =
  { run =
      function
      | `O assoc -> Ok (List.map fst assoc)
      | json -> (fail "Expected an object").run json
  }

let key_value_pairs : 'a decoder -> (string * 'a) list decoder = fun decoder ->
  { run =
      function
      | `O assoc ->
        assoc
        |> List.map
          CCResult.Infix.(fun (key, value_json) ->
              decoder.run value_json >|= fun value -> (key, value)
            )
        |> combine_errors
        |> CCResult.map_err
          (tag_errors (Printf.sprintf "Failed while decoding an object"))
      | json -> (fail "Expected an object").run json
  }

let key_value_pairs_seq : (string -> 'a decoder) -> 'a list decoder = fun decoder ->
  { run =
      function
      | `O assoc ->
        assoc
        |> List.map (fun (key, value_json) ->
            (decoder key).run value_json
          )
        |> combine_errors
        |> CCResult.map_err
          (tag_errors (Printf.sprintf "Failed while decoding an object"))
      | json -> (fail "Expected an object").run json
  }
