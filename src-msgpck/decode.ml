open Decoders
module M = Msgpck

module Msgpck_decodeable : Decode.Decodeable with type value = Msgpck.t = struct
  type value = Msgpck.t

  let pp fmt t = Format.fprintf fmt "@[%a@]" Msgpck.pp t

  let of_string (input : string) : (value, string) result =
    try Ok (snd @@ M.StringBuf.read input) with Invalid_argument s -> Error s


  let of_file (file : string) : (value, string) result =
    try
      Ok
        (Util.with_file_in file (fun chan ->
             Util.read_all chan |> M.StringBuf.read |> snd ) )
    with
    | e ->
        Error (Printexc.to_string e)


  let get_string = function M.String str | M.Bytes str -> Some str | _ -> None

  (* note: the other int constructors are only used for values that do
     not fit in [int]. *)
  let get_int = function M.Int int -> Some int | _ -> None

  let get_float = function
    | M.Float float ->
        Some float
    | M.Float32 f ->
        Some (Int32.float_of_bits f)
    | _ ->
        None


  let get_null = function M.Nil -> Some () | _ -> None

  let get_bool = function M.Bool bool -> Some bool | _ -> None

  let get_list = function M.List a -> Some a | _ -> None

  let get_key_value_pairs = function M.Map assoc -> Some assoc | _ -> None

  let to_list vs = M.List vs
end

include Decode.Make (Msgpck_decodeable)

let string_strict : string decoder =
  { Decoder.dec =
      (function
      | M.String b -> Ok b | m -> (fail "Expected string (strict)").dec m )
  }


let bytes : string decoder =
  { Decoder.dec =
      (function M.Bytes b -> Ok b | m -> (fail "Expected bytes").dec m)
  }


let int32 : _ decoder =
  { Decoder.dec =
      (function M.Int32 i -> Ok i | m -> (fail "Expected int32").dec m)
  }


let int64 : _ decoder =
  { Decoder.dec =
      (function M.Int64 i -> Ok i | m -> (fail "Expected int64").dec m)
  }


let uint32 : _ decoder =
  { Decoder.dec =
      (function M.Uint32 i -> Ok i | m -> (fail "Expected uint32").dec m)
  }


let uint64 : _ decoder =
  { Decoder.dec =
      (function M.Uint64 i -> Ok i | m -> (fail "Expected uint64").dec m)
  }


let ext : (int * string) decoder =
  { Decoder.dec =
      (function
      | M.Ext (i, s) -> Ok (i, s) | m -> (fail "Expected extension").dec m )
  }
