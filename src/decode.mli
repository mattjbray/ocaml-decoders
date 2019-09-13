type 'value exposed_error =
  | Decoder_error of string * 'value option
  | Decoder_errors of 'value exposed_error list
  | Decoder_tag of string * 'value exposed_error

type ('good, 'bad) result = ('good, 'bad) Decoders_util.My_result.t = Ok of 'good | Error of 'bad

type ('value, 'a) exposed_decoder = { run : 'value -> ('a, 'value exposed_error) result }

(** User-facing Decoder interface. *)
module type S = sig
  (** The type of values to be decoded (e.g. JSON or Yaml). *)
  type value

  type error = value exposed_error

  val pp_error : Format.formatter -> error -> unit
  val string_of_error : error -> string

  val of_string : string -> (value, error) result
  val of_file : string -> (value, error) result

  (** The type of decoders.

      Use the functions below to construct decoders for your data types.

      To run a decoder, pass it to {!val:decode_value}.
  *)
  type 'a decoder

  (** {2 Primitives} *)

  (** Decode a [string]. *)
  val string : string decoder

  (** Decode an [int]. *)
  val int : int decoder

  (** Decode a [float]. *)
  val float : float decoder

  (** Decode a [bool]. *)
  val bool : bool decoder

  (** Decode a literal [value]. *)
  val value : value decoder

  (** {2 Lists} *)

  (** Decode a collection into an OCaml list. *)
  val list : 'a decoder -> 'a list decoder

  (** Decode a collection into an OCaml list, skipping elements for which the
      decoder returns None.
  *)
  val list_filter : 'a option decoder -> 'a list decoder

  (** Decode a collection with an accumulator.

      If we consider that an ['a decoder] is basically a type alias for
      [json -> ('a, error) result], the signature of this function is comparable
      to that of [List.fold_left]:

      {[
      val List.fold_left : ('a ->   'b ->                 'a) -> 'a -> 'b list ->                 'a
      val list_fold_left : ('a -> json -> ('a, error) result) -> 'a ->    json -> ('a, error) result
      val list_fold_left : ('a ->                 'a decoder) -> 'a ->                    'a decoder
      ]}
  *)
  val list_fold_left: ('a -> 'a decoder) -> 'a -> 'a decoder

  (** Decode a collection, requiring a particular index. *)
  val index : int -> 'a decoder -> 'a decoder

  (** [fst |> uncons rest] decodes the first element of a list using [fst], then
      decodes the remainder of the list using [rest].

      For example, to decode this s-expression:

          (library
            (name decoders))

      we can use this decoder:

          string |> uncons (function
            | "library" -> field "name" string
            | _ -> fail "Expected a library stanza")

      As another example, say you have a JSON array that starts with a string,
      then a bool, then a list of integers:

          ["hello", true, 1, 2, 3, 4]

      We could decode it like this:

          let (>>=::) fst rest = uncons rest fst

          let decoder : (string * bool * int list) decoder =
            string >>=:: fun the_string ->
            bool >>=:: fun the_bool ->
            list int >>= fun the_ints ->
            succeed (the_string, the_bool, the_ints)

      (If you squint, the uncons operator [>>=::] kind of looks like the cons
      operator [::].)
  *)
  val uncons : ('a -> 'b decoder) -> 'a decoder -> 'b decoder

  (** {1 Object primitives} *)

  (** Decode an object, requiring a particular field. *)
  val field : string -> 'a decoder -> 'a decoder

  (** Decode an object, where a particular field may or may not be present.

      For example, [(field_opt "hello" int)]:

      - when run on [{"hello": 123}], will succeed with [Some 123]
      - when run on [{"hello": null}], will fail
      - when run on [{"world": 123}], will succeed with [None]
      - when run on [["a", "list", "of", "strings"]], will fail
  *)
  val field_opt : string -> 'a decoder -> 'a option decoder

  (** Decode an object, requiring exactly one field. *)
  val single_field : (string -> 'a decoder) -> 'a decoder

  (** Decode a nested object, requiring certain fields. *)
  val at : string list -> 'a decoder -> 'a decoder

  (** {2 Inconsistent structure} *)

  (** [maybe d] is a decoder that always succeeds. If [d] succeeds with [x],
      then [maybe d] succeeds with [Some x], otherwise if [d] fails, then [maybe d]
      succeeds with [None].

      For example, [maybe (field "hello" int)]:

      - when run on [{"hello": 123}], will succeed with [Some 123]
      - when run on [{"hello": null}], will succeed with [None]
      - when run on [{"world": 123}], will succeed with [None]
      - when run on [["a", "list", "of", "strings"]], will succeed with [None]

  *)
  val maybe : 'a decoder -> 'a option decoder

  (** [nullable d] will succeed with [None] if the JSON value is [null]. If the
      JSON value is non-[null], it wraps the result of running [x] in a [Some].

      For example, [field "hello" (nullable int)]:

      - when run on [{"hello": 123}], will succeed with [Some 123]
      - when run on [{"hello": null}], will succeed with [None]
      - when run on [{"world": 123}], will fail
      - when run on [["a", "list", "of", "strings"]], will fail

  *)
  val nullable : 'a decoder -> 'a option decoder

  (** Try a sequence of different decoders. *)
  val one_of : (string * 'a decoder) list -> 'a decoder

  (** {2 Mapping} *)

  (** Map over the result of a decoder. *)
  val map : ('a -> 'b) -> 'a decoder -> 'b decoder

  (** Try two decoders and then combine the result. We can use this to decode
      objects with many fields (but it's preferable to use [Infix.(>>=)] - see the README).
  *)
  val apply : ('a -> 'b) decoder -> 'a decoder -> 'b decoder

  (** {2 Working with object keys} *)

  (** Decode all of the keys of an object to a list of strings. *)
  val keys : string list decoder

  (** Decode an object into a list of key-value pairs. *)
  val key_value_pairs : 'v decoder -> (string * 'v) list decoder

  (** Decode an object into a list of values, where the value
      decoder depends on the key. *)
  val key_value_pairs_seq : (string -> 'v decoder) -> 'v list decoder

  (** [keys'] is for when your keys might not be strings - probably only likely for Yaml. *)
  val keys' : 'k decoder -> 'k list decoder
  val key_value_pairs' : 'k decoder -> 'v decoder -> ('k * 'v) list decoder
  val key_value_pairs_seq' : 'k decoder -> ('k -> 'v decoder) -> 'v list decoder

  (** {2 Fancy decoding} *)

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

  include module type of Infix

  (** {2 Running decoders} *)

  (** Run a decoder on some input. *)
  val decode_value : 'a decoder -> value -> ('a, error) result

  (** Run a decoder on a string. *)
  val decode_string : 'a decoder -> string -> ('a, error) result

  (** Run a decoder on a file. *)
  val decode_file : 'a decoder -> string -> ('a, error) result

  (** {2 Pipeline Decoders} *)
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
    val required_at : string list -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder
    val optional : string -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder
    val optional_at : string list -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder
    val custom : 'a decoder -> ('a -> 'b) decoder -> 'b decoder
  end
end

(** {2 Creating a Decoder implementation}

    The following is useful only if you are creating a new Decoder implementation.
*)

(** Signature of things that can be decoded. *)
module type Decodeable = sig
  type value
  val pp : Format.formatter -> value -> unit
  val of_string : string -> (value, string) result
  val of_file : string -> (value, string) result

  val get_string : value -> string option
  val get_int : value -> int option
  val get_float : value -> float option
  val get_bool : value -> bool option
  val get_null : value -> unit option
  val get_list : value -> value list option
  val get_key_value_pairs : value -> (value * value) list option

  val to_list : value list -> value
end

(** Derive decoders for a [Decodeable.value]. *)
module Make(M : Decodeable) : S with type value = M.value
                                 and type 'a decoder = (M.value, 'a) exposed_decoder
