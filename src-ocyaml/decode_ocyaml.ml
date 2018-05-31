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

  let get_string : t -> string option = function
    | Scalar value -> Some value
    | _ -> None

  let get_int : t -> int option =
    fun t ->
      try
        get_string t
        |> CCOpt.map int_of_string
      with
      | Failure _ -> None


  let get_float : t -> float option =
    fun t ->
      try
        get_string t
        |> CCOpt.map float_of_string
      with
      | Failure _ -> None

  let get_bool : t -> bool option =
    fun t ->
      try
        get_string t
        |> CCOpt.map bool_of_string
      with
      | Failure _ -> None

  let get_null : t -> unit option =
    fun t ->
      get_string t
      |> CCOpt.flat_map (function
          | "" -> Some ()
          | _ -> None
        )

  let get_list = function
    | Collection l -> Some l
    | _ -> None

  let get_field key = function
    | Structure assoc -> CCList.assoc_opt ~eq:Ocyaml.equal (Scalar key) assoc
    | _ -> None

  let get_single_field = function
    | Structure [(Scalar key, value)] -> Some (key, value)
    | _ -> None

  let of_string : string -> (t, string) result =
    fun string ->
      try Ok (Ocyaml.of_string string) with
      | exn -> Error (Printexc.to_string exn)
end

open Ocyaml

module M = Decode.Make(Yaml_decodeable)

include M

let keys_yaml : 'k decoder -> 'k list decoder = fun decoder ->
  { run =
      function
      | Structure assoc ->
        (List.map (fun (key_yaml, _) -> decoder.run key_yaml) assoc)
        |> combine_errors
        |> CCResult.map_err
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
            CCResult.Infix.(fun (key_yaml, value_yaml) ->
                key_decoder.run key_yaml >>= fun key ->
                value_decoder.run value_yaml >|= fun value ->
                (key, value))
          |> combine_errors
          |> CCResult.map_err
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
            CCResult.Infix.(fun (key_yaml, value_yaml) ->
                key_decoder.run key_yaml >>= fun key ->
                (value_decoder key).run value_yaml)
          |> combine_errors
          |> CCResult.map_err
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
