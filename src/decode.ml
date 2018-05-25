(** Functors for creating Decoders. *)

module Util = Util

(** Signature of things that can be decoded. *)
module type Decodeable = sig
  type t
  val pp : Format.formatter -> t -> unit
end

(** Basic decoder combinators. *)
module type Basic = sig
  include Decodeable

  type error =
    | Decoder_error of string * t
    | Decoder_tag of string * error list

  val pp_error : Format.formatter -> error -> unit
  val tag_error : string -> error -> error
  val tag_errors : string -> error list -> error
  val combine_errors : ('a, error) result list -> ('a list, error list) result

  type 'a decoder = { run : t -> ('a, error) result }

  val succeed : 'a -> 'a decoder
  val fail : string -> 'a decoder
  val fail_with : error -> 'a decoder
  val from_result : ('a, error) result -> 'a decoder
  val value : t decoder
  val map : ('a -> 'b) -> 'a decoder -> 'b decoder
  val apply : ('a -> 'b) decoder -> 'a decoder -> 'b decoder
  val and_then : ('a -> 'b decoder) -> 'a decoder -> 'b decoder
  val fix : ('a decoder -> 'a decoder) -> 'a decoder
  val decode_value : 'a decoder -> t -> ('a, error) result
  val maybe : 'a decoder -> 'a option decoder
  val one_of : (string * 'a decoder) list -> 'a decoder
end

module Make_Basic(Decodeable : Decodeable) : Basic with type t = Decodeable.t = struct
  include Decodeable
  open Util.Result.Infix

  type error =
    | Decoder_error of string * t
    | Decoder_tag of string * error list

  let rec pp_error fmt = function
    | Decoder_error (msg, t) -> Format.fprintf fmt "@[%s, but got@ @[%a@]@]" msg pp t
    | Decoder_tag (msg, errors) ->
      Format.fprintf fmt
        "@[<2>%s:@ @[%a@]@]"
        msg
        (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_error)
        errors

  let tag_error (msg : string) (error : error) : error =
    Decoder_tag (msg, [ error ])

  let tag_errors (msg : string) (errors : error list) : error =
    Decoder_tag (msg, errors)

  let rec combine_errors : ('a, error) result list -> ('a list, error list) result =
    function
    | [] -> Ok []
    | result :: rest ->
      begin match result, combine_errors rest with
        | Ok x, Ok xs -> Ok (x :: xs)
        | Error e, Error es -> Error (e :: es)
        | Error e, Ok _ -> Error [e]
        | Ok _, Error es -> Error es
      end

  type 'a decoder = { run : t -> ('a, error) result }

  let succeed x =
    { run = fun _ -> Ok x }

  let fail msg =
    { run = fun input -> Error (Decoder_error (msg, input)) }

  let fail_with error =
    { run = fun _ -> Error error }

  let from_result = function
    | Ok ok -> succeed ok
    | Error error -> fail_with error

  let value =
    { run = fun input -> Ok input }

  let map f decoder =
    { run = fun input -> decoder.run input >>| f }

  let apply : ('a -> 'b) decoder -> 'a decoder -> 'b decoder =
    fun f decoder ->
      { run =
          fun input ->
            match f.run input, decoder.run input with
            | Error e1, Error e2 -> Error (tag_errors "apply" [e1; e2]) (* TODO *)
            | Error e, _ -> Error e
            | _, Error e -> Error e
            | Ok g, Ok x -> Ok (g x)
      }

  let and_then (f : 'a -> 'b decoder) (decoder : 'a decoder) : 'b decoder =
    { run = fun input ->
      decoder.run input >>= fun result ->
      (f result).run input
    }

  let fix (f : 'a decoder -> 'a decoder) : 'a decoder =
    let rec p = lazy (f r)
    and r = { run = fun value -> (Lazy.force p).run value }
    in
    r

  let decode_value (decoder : 'a decoder) (input : t) : ('a, error) result =
      decoder.run input

  let maybe (decoder : 'a decoder) : 'a option decoder =
    { run = fun input ->
      match (decoder.run input) with
      | Ok result -> Ok (Some result)
      | Error err -> Ok None
    }

  let one_of : (string * 'a decoder) list -> 'a decoder =
    fun decoders ->
      let run input =
        let rec go errors = function
          | (name, decoder) :: rest ->
            (match decoder.run input with
            | Ok result -> Ok result
            | Error error -> go (tag_errors (Printf.sprintf "%S decoder" name) [ error ] :: errors) rest)
          | [] ->
            Error
              (tag_errors "I tried the following decoders but they all failed" errors)
        in
        go [] decoders
      in { run }
end


(** Implementation-specific Decoders. *)
module type Primitives = sig
  include Basic

  val string : string decoder
  val int : int decoder
  val float : float decoder
  val bool : bool decoder
  val null : 'a -> 'a decoder
  val list : 'a decoder -> 'a list decoder
  val field : string -> 'a decoder -> 'a decoder
  val single_field : (string -> 'a decoder) -> 'a decoder
  val index : int -> 'a decoder -> 'a decoder
  val of_string : string -> (t, error) result
end

module type S_exposed = sig
  include Primitives

  val at : string list -> 'a decoder -> 'a decoder
  val decode : 'a -> 'a decoder
  val required : string -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder
  val requiredAt : string list -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder
  val optional : string -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder
  val optionalAt : string list -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder
  val custom : 'a decoder -> ('a -> 'b) decoder -> 'b decoder

  val decode_string : 'a decoder -> string -> ('a, error) result
end


module Make(Primitives : Primitives) : S_exposed with type t = Primitives.t = struct
  open Util.Result.Infix

  include Primitives

  let rec at : string list -> 'a decoder -> 'a decoder = fun path decoder ->
    match path with
    | [key] -> field key decoder
    | key :: rest -> field key (at rest decoder)
    | [] -> fail "Must provide at least one key to 'at'"

  let decode = succeed

  let custom : 'a decoder -> ('a -> 'b) decoder -> 'b decoder =
    fun customDecoder next ->
      apply next customDecoder

  let required : string -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder =
    fun key decoder next ->
      custom (field key decoder) next

  let requiredAt : string list -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder =
    fun path decoder next ->
      custom (at path decoder) next

  let optional_decoder : t decoder -> 'a decoder -> 'a -> 'a decoder =
    fun path_decoder val_decoder default ->
      let null_or decoder =
        one_of
          [ ( "non-null", decoder )
          ; ( "null", null default )
          ] in
      let handle_result : t -> 'a decoder =
        fun input ->
          match decode_value path_decoder input with
          | Ok rawValue ->
            (* The field was present. *)
            decode_value (null_or val_decoder) rawValue
            |> from_result
          | Error _ ->
            (* The field was not present. *)
            succeed default
      in
      value |> and_then handle_result

  let optional : string -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder =
    fun key val_decoder default next ->
      custom (optional_decoder (field key value) val_decoder default) next

  let optionalAt : string list -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder =
    fun path val_decoder default next ->
      custom (optional_decoder (at path value) val_decoder default) next

  let decode_string : 'a decoder -> string -> ('a, error) result =
    fun decoder string ->
      of_string string >>= decode_value decoder
end


(** User-facing Decoder interface. *)
module type S = sig
  (* Note: this signature is just S_exposed, but with the type [t] abstract. *)

  (** The type of values to be decoded (e.g. JSON or Yaml). *)
  type t

  type error

  val pp_error : Format.formatter -> error -> unit

  (** The type of decoders.

      Use the functions below to construct decoders for your data types.

      To run a decoder, pass it to {!val:decode_value}.
  *)
  type 'a decoder

  (** {1 Primitives} *)

  (** Decode a [string]. *)
  val string : string decoder

  (** Decode an [int]. *)
  val int : int decoder

  (** Decode a [float]. *)
  val float : float decoder

  (** Decode a [bool]. *)
  val bool : bool decoder

  (** {1 Data structures} *)

  (** Decode a collection into an OCaml list. *)
  val list : 'a decoder -> 'a list decoder

  (** {1 Object primitives} *)

  (** Decode an object, requiring a particular field. *)
  val field : string -> 'a decoder -> 'a decoder

  (** Decode an object, requiring exactly one field. *)
  val single_field : (string -> 'a decoder) -> 'a decoder

  (** Decode an array, requiring a particular index. *)
  val index : int -> 'a decoder -> 'a decoder

  (** Decode a nested object, requiring certain fields. *)
  val at : string list -> 'a decoder -> 'a decoder

  (** {1 Inconsistent structure} *)

  (** Helpful for dealing with optional fields. *)
  val maybe : 'a decoder -> 'a option decoder

  (** Try a sequence of different decoders. *)
  val one_of : (string * 'a decoder) list -> 'a decoder

  (** {1 Mapping} *)

  (** Map functions are useful for decoding complex objects.

      For example, given an object with structure
      {[
        {
          "name": "Joe"
              "age": 42
        }
      ]}
      we want to decode it to our OCaml type
      {[
        type person =
          { name : string
          ; age : int
          }
      ]}

      We define a helper function to construct values of this type:
      {[
        let as_person name age =
          { name = name
          ; age = age
          }
      ]}

      The decoder looks like this:
      {[
        let person_decoder : person decoder =
          map2 as_person
            (field "name" string)
            (field "age" int)
      ]}
  *)

  (** Transform a decoder. *)
  val map : ('a -> 'b) -> 'a decoder -> 'b decoder

  (** Try two decoders and then combine the result. We can use this to decode
      objects with many fields.
  *)
  val apply : ('a -> 'b) decoder -> 'a decoder -> 'b decoder

  (** {1 Working with object keys} *)

  (** Decode all of the keys of an object to a list of strings. *)
  val keys : string list decoder

  (** Decode an object into a list of key-value pairs. *)
  val key_value_pairs : 'a decoder -> (string * 'a) list decoder

  (** Decode an object into a list of values, where the value
      decoder depends on the key. *)
  val key_value_pairs_seq : (string -> 'a decoder) -> 'a list decoder

  (** Decode an object into a [String.Map.t]. *)
  (* val string_map : 'a decoder -> 'a Core.String.Map.t decoder *)

  (** {1 Fancy decoding} *)

  (** A decoder that always succeeds with the argument, ignoring the input. *)
  val succeed : 'a -> 'a decoder

  (** A decoder that always fails with the given message, ignoring the input. *)
  val fail : string -> 'a decoder

  val fail_with : error -> 'a decoder

  val from_result : ('a, error) result -> 'a decoder

  (** Create decoders that depend on previous results. *)
  val and_then : ('a -> 'b decoder) -> 'a decoder -> 'b decoder

  (** Recursive decoders.

      [let my_decoder = fix (fun my_decoder -> ...)] allows you to define
      [my_decoder] in terms of itself.
  *)
  val fix : ('a decoder -> 'a decoder) -> 'a decoder

  (** {1 Running decoders} *)

  (** Run a decoder on some input. *)
  val decode_value : 'a decoder -> t -> ('a, error) result

  (** Run a decoder on a string. *)
  val decode_string : 'a decoder -> string -> ('a, error) result

  (** {1 Pipeline Decoders} *)

  (**
      Pipeline decoders present an alternative to the [mapN] style. They read
      more naturally, but can lead to harder-to-understand type errors.
     {[
       let person_decoder : person decoder =
         decode as_person
         |> required "name" string
         |> required "age" int
     ]}
  *)

  val decode : 'a -> 'a decoder
  val required : string -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder
  val requiredAt : string list -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder
  val optional : string -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder
  val optionalAt : string list -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder
  val custom : 'a decoder -> ('a -> 'b) decoder -> 'b decoder
end
