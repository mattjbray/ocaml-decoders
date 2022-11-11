open Decoders

module Cbor_decodeable : Decode.Decodeable with type value = CBOR.Simple.t =
struct
  type value = CBOR.Simple.t

  let pp fmt t = Format.fprintf fmt "@[%s@]" (CBOR.Simple.to_diagnostic t)

  let of_string (input : string) : (value, string) result =
    try Ok (CBOR.Simple.decode input) with CBOR.Error msg -> Error msg


  let of_file (file : string) : (value, string) result =
    try
      Ok
        (Util.with_file_in file (fun chan ->
             Util.read_all chan |> CBOR.Simple.decode ) )
    with
    | e ->
        Error (Printexc.to_string e)


  let get_string = function `Text str -> Some str | _ -> None

  let get_int = function `Int int -> Some int | _ -> None

  let get_float = function `Float float -> Some float | _ -> None

  let get_null = function `Null -> Some () | _ -> None

  let get_bool = function `Bool bool -> Some bool | _ -> None

  let get_list = function `Array a -> Some a | _ -> None

  let get_key_value_pairs = function `Map assoc -> Some assoc | _ -> None

  let to_list vs = `Array vs
end

include Decode.Make (Cbor_decodeable)

(* CBOR-specific decoders *)

let undefined : unit decoder ={Decoder.dec= function
  | `Undefined ->
      Ok ()
  | json ->
      (fail "Expected Undefined").dec json
}


let simple : int decoder = {Decoder.dec=function
  | `Simple i ->
      Ok i
  | json ->
      (fail "Expected Simple").dec json
}


let bytes : string decoder = {Decoder.dec=function
  | `Bytes b ->
      Ok b
  | json ->
      (fail "Expected bytes").dec json
}
