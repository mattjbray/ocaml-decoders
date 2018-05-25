(** {2 Ocyaml implementation} *)

module Yaml_decodeable : Decode.Decodeable with type t = Ocyaml.yaml = struct
  open Ocyaml

  type t = yaml

  let rec pp fmt = function
    | Scalar string -> Format.fprintf fmt "@[%S@]" string
    | Collection xs ->
      Format.fprintf fmt "@[%a@]"
        (Format.pp_print_list (fun fmt yaml -> Format.fprintf fmt "- @[%a@]" pp yaml))
        xs
    | Structure xs ->
      Format.fprintf fmt "@[%a@]"
        (Format.pp_print_list (fun fmt (key, value) ->
             Format.fprintf fmt "@[%a@]:@ @[%a@]" pp key pp value))
        xs
end


module Yaml_primitives : (Decode.Primitives with type t = Ocyaml.yaml) = struct
  open Ocyaml

  include Decode.Make_Basic(Yaml_decodeable)

  let string : string decoder =
    { run =
        function
        | Scalar value -> Ok value
        | yaml -> (fail "Expected a string").run yaml
    }

  let int : int decoder =
    string |> and_then
      (fun s ->
         try succeed (int_of_string s) with
         | Failure _ -> fail "Expected an integer")

  let float : float decoder =
    string |> and_then
      (fun s ->
         try succeed (float_of_string s) with
         | Failure _ -> fail "Expected a float")

  let bool : bool decoder =
    string |> and_then
      (fun s ->
         try succeed (bool_of_string s) with
         | Failure _ -> fail "Expected a bool")

  let null : 'a -> 'a decoder = fun x ->
    string |> and_then
      (fun s ->
         if String.length s = 0 then
           succeed x
         else
           fail "Expected a null")

  let list : 'a decoder -> 'a list decoder = fun decoder ->
    { run =
        function
        | Collection l ->
          l
        |> List.map decoder.run
        |> combine_errors
        |> Decode.Util.Result.map_error
          (tag_errors "Failed while decoding a list item")
        | yaml -> (fail "Expected a list").run yaml
    }

  let field : string -> 'a decoder -> 'a decoder = fun key decoder ->
    { run = fun yaml ->
        match yaml with
        | Structure assoc ->
          let sub_yaml =
            try Some (List.assoc (Scalar key) assoc) with
            | Not_found -> None
          in
          begin match sub_yaml with
            | Some sub_yaml ->
              decoder.run sub_yaml
              |> Decode.Util.Result.map_error
                (tag_error ("'" ^ key ^ "':"))
           | None -> (fail ("Expected object to have an attribute '" ^ key ^ "'")).run yaml
          end
        | yaml -> (fail "Expected an object").run yaml
    }

  let single_field : (string -> 'a decoder) -> 'a decoder = fun decoder ->
    { run =
        function
        | Structure [(Scalar key, value_yaml)] ->
          (decoder key).run value_yaml
          |> Decode.Util.Result.map_error
            (tag_error ("'" ^ key ^ "':"))
        | yaml -> (fail "Expected an object with a single attribute").run yaml
    }

  let index : int -> 'a decoder -> 'a decoder = fun i decoder ->
    { run = fun yaml ->
        match yaml with
        | Collection l ->
          let item =
            try Some (List.nth l i) with
            | Failure _-> None
            | Invalid_argument _ -> None
          in
          (match item with
            | None -> (fail ("expected a list with at least " ^ string_of_int i ^ " elements")).run yaml
            | Some item ->
              decoder.run item
              |> Decode.Util.Result.map_error
                (tag_error (Printf.sprintf "While decoding item at index %d" i))
          )
        | _ -> (fail "Expected a list").run yaml
    }

  let of_string : string -> (t, error) result =
    fun string ->
      try Ok (Ocyaml.of_string string) with
      | exn -> Error (Decoder_error (Printexc.to_string exn, Scalar ""))
end


open Ocyaml

module M = Decode.Make(Yaml_primitives)

include M

let keys_yaml : 'k decoder -> 'k list decoder = fun decoder ->
  { run =
      function
      | Structure assoc ->
        (List.map (fun (key_yaml, _) -> decoder.run key_yaml) assoc)
        |> combine_errors
        |> Decode.Util.Result.map_error
          (tag_errors "Failed while decoding the keys of an object")
      | yaml -> (fail "Expected an object").run yaml
  }

let keys : string list decoder =
  keys_yaml string

let key_value_pairs_yaml : 'k decoder -> 'v decoder -> ('k * 'v) list decoder =
  fun key_decoder value_decoder ->
    { run =
        function
        | Structure assoc ->
          assoc
          |> List.map
            Decode.Util.Result.Infix.(fun (key_yaml, value_yaml) ->
                key_decoder.run key_yaml >>= fun key ->
                value_decoder.run value_yaml >>| fun value ->
                (key, value))
          |> combine_errors
          |> Decode.Util.Result.map_error
            (tag_errors "Failed while decoding key-value pairs")
        | yaml -> (fail "Expected an object").run yaml
    }

let key_value_pairs : 'v decoder -> (string * 'v) list decoder =
  fun value_decoder ->
    key_value_pairs_yaml string value_decoder

let key_value_pairs_seq_yaml : 'k decoder -> ('k -> 'v decoder) -> 'v list decoder =
  fun key_decoder value_decoder ->
    { run =
        function
        | Structure assoc ->
          assoc
          |> List.map
            Decode.Util.Result.Infix.(fun (key_yaml, value_yaml) ->
                key_decoder.run key_yaml >>= fun key ->
                (value_decoder key).run value_yaml)
          |> combine_errors
          |> Decode.Util.Result.map_error
            (tag_errors "Failed while decoding key-value pairs")
        | yaml -> (fail "Expected an object").run yaml
    }

let key_value_pairs_seq : (string -> 'v decoder) -> 'v list decoder =
  fun value_decoder ->
    key_value_pairs_seq_yaml string value_decoder

(* let string_map : 'a decoder -> 'a String.Map.t decoder = *)
(*   fun value_decoder -> *)
(*     key_value_pairs value_decoder *)
(*     |> and_then *)
(*       (fun assoc_list -> *)
(*          match String.Map.of_alist assoc_list with *)
(*          | `Duplicate_key key -> fail @@ sprintf "Duplicate key: '%s'" key *)
(*          | `Ok result -> succeed result *)
(*       ) *)
