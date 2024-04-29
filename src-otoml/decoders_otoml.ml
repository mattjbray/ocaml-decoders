(** {2 Yojson implementation} *)

open Decoders

module Make
    (Toml : Otoml.Base.TomlImplementation
              with type toml_integer = int
               and type toml_float = float) =
struct
  module T = Toml

  module Toml_decodeable : Decode.Decodeable with type value = T.t = struct
    type value = Toml.t

    let pp fmt v = Format.fprintf fmt "@[%s@]" (T.Printer.to_string v)

    let of_string : string -> (value, string) result =
     fun string -> T.Parser.from_string_result string


    let of_file file = T.Parser.from_file_result file

    let get_string = function T.TomlString value -> Some value | _ -> None

    let get_int = function T.TomlInteger value -> Some value | _ -> None

    let get_float = function
      | T.TomlFloat value ->
          Some value
      | T.TomlInteger value ->
          Some (float_of_int value)
      | _ ->
          None


    let get_bool = function T.TomlBoolean value -> Some value | _ -> None

    let get_null = function T.TomlString "" -> Some () | _ -> None

    let get_list : value -> value list option = function
      | T.TomlArray l | T.TomlTableArray l ->
          Some l
      | _ ->
          None


    let get_key_value_pairs : value -> (value * value) list option = function
      | T.TomlTable assoc | T.TomlInlineTable assoc ->
          Some (List.map (fun (key, value) -> (T.string key, value)) assoc)
      | _ ->
          None


    let to_list values = T.array values
  end

  module Decode = Decode.Make (Toml_decodeable)

  module Toml_encodeable = struct
    type value = Toml.t

    let to_string v = Toml.Printer.to_string v

    let of_string = Toml.string

    let of_int = Toml.integer

    let of_float = Toml.float

    let of_bool = Toml.boolean

    let null = Toml.string ""

    let of_list = Toml.array

    let of_key_value_pairs xs =
      Toml.inline_table
        ( xs
        |> Util.My_list.filter_map (fun (k, v) ->
               match k with Toml.TomlString k -> Some (k, v) | _ -> None ) )
  end

  module Encode = Encode.Make (Toml_encodeable)
end

include Make (Otoml)
