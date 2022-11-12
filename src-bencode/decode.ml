open Decoders

module Bencode_decodeable : Decode.Decodeable with type value = Bencode.t =
struct
  type value = Bencode.t

  let pp fmt t = Format.fprintf fmt "@[%s@]" (Bencode.pretty_print t)

  let of_string (input : string) : (value, string) result =
    try Ok (Bencode.decode (`String input)) with _ -> Error "invalid bencode"


  let of_file (file : string) : (value, string) result =
    try
      let v = Util.with_file_in file (fun ic -> Bencode.decode (`Channel ic)) in
      Ok v
    with
    | e ->
        Error (Printexc.to_string e)


  let get_string = function Bencode.String str -> Some str | _ -> None

  let get_int = function
    | Bencode.Integer int ->
        Some (Int64.to_int int)
    | Bencode.String s ->
      (try Some (int_of_string s) with _ -> None)
    | _ ->
        None


  let get_float = function
    | Bencode.String s ->
      (try Some (float_of_string s) with _ -> None)
    | _ ->
        None


  let get_null = function
    | Bencode.Integer 0L | Bencode.List [] ->
        Some ()
    | _ ->
        None


  let get_bool = function
    | Bencode.Integer 1L | Bencode.String "true" ->
        Some true
    | Bencode.Integer 0L | Bencode.String "false" ->
        Some false
    | _ ->
        None


  let get_list = function Bencode.List a -> Some a | _ -> None

  let get_key_value_pairs = function
    | Bencode.Dict assoc ->
        Some (List.rev_map (fun (s, v) -> (Bencode.String s, v)) assoc)
    | _ ->
        None


  let to_list vs = Bencode.List vs
end

include Decode.Make (Bencode_decodeable)

let int64 : int64 decoder =
  { Decoder.dec =
      (fun t ->
        match t with
        | Bencode.Integer value ->
            Ok value
        | _ ->
            (fail "Expected an int64").dec t )
  }
