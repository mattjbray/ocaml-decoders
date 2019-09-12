(** {2 Ocyaml implementation} *)

open Decoders

module Yaml_decodeable : Decode.Decodeable with type value = Ocyaml.yaml = struct
  open Ocyaml

  type value = yaml

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

  let of_string : string -> (value, string) result =
    fun string ->
      try Ok (Ocyaml.of_string string) with
      | exn -> Error (Printexc.to_string exn)

  let of_file file =
    try Ok (Ocyaml.of_file file) with
    | e -> Error (Printexc.to_string e)

  let get_string : value -> string option = function
    | Scalar value -> Some value
    | _ -> None

  let get_int : value -> int option =
    fun t ->
      try
        get_string t
        |> CCOpt.map int_of_string
      with
      | Failure _ -> None


  let get_float : value -> float option =
    fun t ->
      try
        get_string t
        |> CCOpt.map float_of_string
      with
      | Failure _ -> None

  let get_bool : value -> bool option =
    fun t ->
      try
        get_string t
        |> CCOpt.map bool_of_string
      with
      | Failure _ -> None

  let get_null : value -> unit option =
    fun t ->
      get_string t
      |> CCOpt.flat_map (function
          | "" -> Some ()
          | _ -> None
        )

  let get_list = function
    | Collection l -> Some l
    | _ -> None

  let get_key_value_pairs = function
    | Structure assoc -> Some assoc
    | _ -> None

  let to_list values = Collection values
end

include Decode.Make(Yaml_decodeable)
