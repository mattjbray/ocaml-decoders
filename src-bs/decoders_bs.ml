(** {2 Bucklescript Js.Json implementation} *)

open Decoders

type ('good, 'bad) result = ('good, 'bad) Decode.result =
  | Ok of 'good
  | Error of 'bad

module Json_decodeable : Decode.Decodeable with type value = Js.Json.t = struct
  type value = Js.Json.t

  let pp fmt json =
    Format.fprintf fmt "@[%s@]" (Js.Json.stringifyWithSpace json 2)


  let of_string : string -> (value, string) result =
   fun string ->
    try Ok (Js.Json.parseExn string) with
    | Js.Exn.Error e ->
        Error (Js.Exn.message e |. Belt.Option.getWithDefault "unknown")


  let of_file _file = failwith "Not implemented"

  let get_string = Js.Json.decodeString

  let get_int json = Js.Json.decodeNumber json |. Belt.Option.map int_of_float

  let get_float = Js.Json.decodeNumber

  let get_bool = Js.Json.decodeBoolean

  let get_null value = Js.Json.decodeNull value |. Belt.Option.map (fun _ -> ())

  let get_list (value : value) : value list option =
    Js.Json.decodeArray value |. Belt.Option.map Array.to_list


  let get_key_value_pairs (value : value) : (value * value) list option =
    Js.Json.decodeObject value
    |. Belt.Option.map (fun dict ->
           Js.Dict.entries dict
           |. Array.to_list
           |> List.map (fun (key, value) -> (Js.Json.string key, value)))


  let to_list values = Js.Json.array (Array.of_list values)
end

module Decode = struct
  module D = Decode.Make (Json_decodeable)
  include D

  let tag_error (msg : string) (error : error) : error = Decoder_tag (msg, error)

  let combine_errors (results : ('a, error) result array) :
      ('a array, error list) result =
    if Js.Array.some Belt.Result.isError results
    then
      Error
        ( results
        |. Belt.Array.keepMap (function Ok _ -> None | Error e -> Some e)
        |> Array.to_list )
    else
      Ok
        ( results
        |> Js.Array.map (function
               | Ok x ->
                   x
               | Error _ ->
                   failwith "Errors should be filtered out here") )


  let tag_errors (msg : string) (errors : error list) : error =
    Decoder_tag (msg, Decoder_errors errors)


  let array : 'a decoder -> 'a array decoder =
   fun decoder ->
    { run =
        (fun t ->
          match Js.Json.decodeArray t with
          | None ->
              (fail "Expected an array").run t
          | Some arr ->
              let res =
                arr
                |> Js.Array.mapi (fun x i ->
                       match decoder.run x with
                       | Ok a ->
                           Ok a
                       | Error e ->
                           Error (tag_error ("element " ^ Js.Int.toString i) e))
                |> combine_errors
              in
              ( match res with
              | Ok a ->
                  Ok a
              | Error e ->
                  Error (tag_errors "while decoding an array" e) ))
    }
end

module Json_encodeable = struct
  type value = Js.Json.t

  let to_string json = Js.Json.stringify json

  let of_string x = Js.Json.string x

  let of_int x = Js.Json.number (float_of_int x)

  let of_float x = Js.Json.number x

  let of_bool x = Js.Json.boolean x

  let null = Js.Json.null

  let of_list xs = Js.Json.array (Array.of_list xs)

  let of_key_value_pairs xs =
    Js.Json.object_
      ( xs
      |. Belt.List.keepMap (fun (k, v) ->
             Js.Json.decodeString k |. Belt.Option.map (fun k -> (k, v)))
      |. Js.Dict.fromList )
end

module Encode = struct
  include Encode.Make (Json_encodeable)

  let array encoder xs =
    xs |> Js.Array.map (fun x -> encoder x) |> Js.Json.array
end
