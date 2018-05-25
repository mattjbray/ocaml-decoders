(** {2 Yojson implementation} *)

module Json_decodeable : Decode.Decodeable with type t = Yojson.Basic.json = struct
  type t = Yojson.Basic.json
end


module Json_primitives : (Decode.Primitives with type t = Yojson.Basic.json) = struct
  open Yojson.Basic

  include Decode.Make_Basic(Json_decodeable)

  let string : string decoder =
    { run = function
      | `String value -> Ok value
      | json -> (fail "Expected a string").run json
    }

  let int : int decoder =
    { run = function
      | `Int value -> Ok value
      | json -> (fail "Expected an int").run json
    }

  let float : float decoder =
    { run = function
      | `Float value -> Ok value
      | `Int value -> Ok (float_of_int value)
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
        List.map decoder.run l
        |> combine_errors
        |> Decode.Util.Result.map_error
           (tag_errors "Failed while decoding a list item")
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
            |> Decode.Util.Result.map_error (tag_error ("'" ^ key ^ "':"))
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
      try Ok (Yojson.Basic.from_string string) with
      | Yojson.Json_error error ->
        Error (Decoder_tag ("Json parse error", [ Decoder_error (error, `Null) ]))
end

include Decode.Make(Json_primitives)

open Yojson.Basic

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

(* let string_map : 'a decoder -> 'a String.Map.t decoder = *)
(*   fun value_decoder -> *)
(*     key_value_pairs value_decoder *)
(*     |> and_then *)
(*       (fun assoc_list -> *)
(*          match String.Map.of_alist assoc_list with *)
(*          | `Duplicate_key key -> fail @@ sprintf "Duplicate key: '%s'" key *)
(*          | `Ok result -> succeed result *)
(*       ) *)
