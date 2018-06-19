(** User-facing Decoder interface. *)
module type S = sig
  (* Note: this signature is just S_exposed, but with the type [t] abstract. *)

  (** The type of values to be decoded (e.g. JSON or Yaml). *)
  type value

  type error =
    | Decoder_error of string * value option
    | Decoder_errors of error list
    | Decoder_tag of string * error

  val pp_error : Format.formatter -> error -> unit

  val of_string : string -> (value, error) result

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

  val nullable : 'a decoder -> 'a option decoder

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
  val keys : 'a decoder -> 'a list decoder

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

  module Infix : sig
    val (>|=) : 'a decoder -> ('a -> 'b) -> 'b decoder
    val (>>=) : 'a decoder -> ('a -> 'b decoder) -> 'b decoder
    val (<*>) : ('a -> 'b) decoder -> 'a decoder -> 'b decoder
  end

  (** {1 Running decoders} *)

  (** Run a decoder on some input. *)
  val decode_value : 'a decoder -> value -> ('a, error) result

  (** Run a decoder on a string. *)
  val decode_string : 'a decoder -> string -> ('a, error) result

  (** {1 Pipeline Decoders} *)
  module Pipeline : sig
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
end

(** Signature of things that can be decoded. *)
module type Decodeable = sig
  type value
  val pp : Format.formatter -> value -> unit
  val of_string : string -> (value, string) result

  val get_string : value -> string option
  val get_int : value -> int option
  val get_float : value -> float option
  val get_bool : value -> bool option
  val get_null : value -> unit option
  val get_list : value -> value list option
  val get_key_value_pairs : value -> (value * value) list option
  val get_field : string -> value -> value option
  val get_single_field : value -> (string * value) option
end


(** Basic decoder combinators. *)
module type S_exposed = sig
  type value
  val pp : Format.formatter -> value -> unit

  type error =
    | Decoder_error of string * value option
    | Decoder_errors of error list
    | Decoder_tag of string * error

  val pp_error : Format.formatter -> error -> unit
  val tag_error : string -> error -> error
  val tag_errors : string -> error list -> error
  val combine_errors : ('a, error) result list -> ('a list, error list) result

  val of_string : string -> (value, error) result

  type 'a decoder = { run : value -> ('a, error) result }

  val succeed : 'a -> 'a decoder
  val fail : string -> 'a decoder
  val fail_with : error -> 'a decoder
  val from_result : ('a, error) result -> 'a decoder
  val value : value decoder
  val map : ('a -> 'b) -> 'a decoder -> 'b decoder
  val apply : ('a -> 'b) decoder -> 'a decoder -> 'b decoder
  val and_then : ('a -> 'b decoder) -> 'a decoder -> 'b decoder
  val fix : ('a decoder -> 'a decoder) -> 'a decoder
  val decode_value : 'a decoder -> value -> ('a, error) result
  val maybe : 'a decoder -> 'a option decoder
  val nullable : 'a decoder -> 'a option decoder
  val one_of : (string * 'a decoder) list -> 'a decoder

  module Infix : sig
    val (>|=) : 'a decoder -> ('a -> 'b) -> 'b decoder
    val (>>=) : 'a decoder -> ('a -> 'b decoder) -> 'b decoder
    val (<*>) : ('a -> 'b) decoder -> 'a decoder -> 'b decoder
  end

  val string : string decoder
  val int : int decoder
  val float : float decoder
  val bool : bool decoder
  val null : 'a -> 'a decoder
  val list : 'a decoder -> 'a list decoder
  val field : string -> 'a decoder -> 'a decoder
  val single_field : (string -> 'a decoder) -> 'a decoder
  val keys : 'a decoder -> 'a list decoder
  val index : int -> 'a decoder -> 'a decoder
  val at : string list -> 'a decoder -> 'a decoder

  module Pipeline : sig
    val decode : 'a -> 'a decoder
    val required : string -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder
    val requiredAt : string list -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder
    val optional : string -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder
    val optionalAt : string list -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder
    val custom : 'a decoder -> ('a -> 'b) decoder -> 'b decoder
  end

  val decode_string : 'a decoder -> string -> ('a, error) result
end

module Make(M : Decodeable) : S_exposed with type value = M.value
