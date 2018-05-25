(** {2 Yojson implementation} *)

module Json_decodeable : Decode.Decodeable with type t = Yojson.Raw.json = struct
  type t = Yojson.Raw.json
  let pp fmt json = Format.fprintf fmt "@[%s@]" (Yojson.Raw.pretty_to_string json)
end


module Json_primitives : (Decode.Primitives with type t = Yojson.Raw.json) = struct
  open Yojson.Raw

  include Decode.Make_Basic(Json_decodeable)

  let string : string decoder =
    { run = function
      | `Stringlit s ->
        (* Stringlits are wrapped in double-quotes. *)
        Ok (String.sub s 1 (String.length s - 2))
      | json -> (fail "Expected a string").run json
    }

  let int : int decoder =
    { run = function
      | `Intlit value -> Ok (int_of_string value)
      | json -> (fail "Expected an int").run json
    }

  let float : float decoder =
    { run = function
      | `Floatlit value -> Ok (float_of_string value)
      | `Intlit value -> Ok (float_of_string value)
      | json -> (fail "Expected a float").run json
    }

  let bool : bool decoder =
    { run = function
      | `Bool value -> Ok value
      | json -> (fail "Expected a bool").run json
    }

  let null : 'a -> 'a decoder =
    fun default ->
    { run = function
      | `Null -> Ok default
      | json -> (fail "Expected a null").run json
    }

  let list : 'a decoder -> 'a list decoder =
    fun decoder ->
    { run = function
      | `List l ->
        List.mapi (fun i x ->
            decoder.run x
            |> Decode.Util.Result.map_error
              (tag_error (Printf.sprintf "element %i" i))
          ) l
        |> combine_errors
        |> Decode.Util.Result.map_error
          (tag_errors "while decoding a list")
      | json -> (fail "Expected a list").run json
    }

  let field : string -> 'a decoder -> 'a decoder = fun key decoder ->
    { run = fun json ->
    match json with
      | `Assoc assoc ->
        let sub_json =
          try Some (List.assoc key assoc) with
          | Not_found -> None
        in
        begin match sub_json with
          | Some sub_json ->
            decoder.run sub_json
            |> Decode.Util.Result.map_error (tag_error (Printf.sprintf "in field %S" key))
          | None -> (fail ("Expected object to have an attribute '" ^ key ^ "'")).run json
        end
      | _ -> (fail "Expected an object").run json
    }

  let single_field : (string -> 'a decoder) -> 'a decoder = fun decoder ->
    { run = function
      | `Assoc [(key, value_json)] ->
        (decoder key).run value_json
        |> Decode.Util.Result.map_error (tag_error ("'" ^ key ^ "':"))
      | json -> (fail "Expected an object with a single attribute").run json
    }

  let index : int -> 'a decoder -> 'a decoder = fun i decoder ->
    { run = fun json ->
      match json with
      | `List l ->
        let item =
          try Some (List.nth l i) with
          | Failure _-> None
          | Invalid_argument _ -> None
        in
        begin match item with
        | None -> (fail ("expected a list with at least " ^ string_of_int i ^ " elements")).run json
        | Some item -> decoder.run item
        end
      | _ -> (fail "Expected a list").run json
    }

  let of_string : string -> (t, error) result =
    fun string ->
      try Ok (Yojson.Raw.from_string string) with
      | Yojson.Json_error error ->
        Error (Decoder_tag ("Json parse error", [ Decoder_error (error, `Null) ]))
end

include Decode.Make(Json_primitives)

open Yojson.Raw

let json_of_file file =
  try Ok (from_file file) with
  | e -> Error (Decoder_error (Printexc.to_string e, `Null))

let keys : string list decoder =
  { run =
      function
      | `Assoc assoc -> Ok (List.map fst assoc)
      | json -> (fail "Expected an object").run json
  }

let key_value_pairs : 'a decoder -> (string * 'a) list decoder = fun decoder ->
  { run =
      function
      | `Assoc assoc ->
        assoc
        |> List.map
          Decode.Util.Result.Infix.(fun (key, value_json) ->
              decoder.run value_json >>| fun value -> (key, value)
            )
        |> combine_errors
        |> Decode.Util.Result.map_error
          (tag_errors (Printf.sprintf "Failed while decoding an object"))
      | json -> (fail "Expected an object").run json
  }

let key_value_pairs_seq : (string -> 'a decoder) -> 'a list decoder = fun decoder ->
  { run =
      function
      | `Assoc assoc ->
        assoc
        |> List.map (fun (key, value_json) ->
            (decoder key).run value_json
          )
        |> combine_errors
        |> Decode.Util.Result.map_error
          (tag_errors (Printf.sprintf "Failed while decoding an object"))
      | json -> (fail "Expected an object").run json
  }

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
