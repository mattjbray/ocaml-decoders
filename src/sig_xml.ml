(** Interface for decoding XML *)
module type S = sig
  (** The type of XML values. *)
  type value

  type error = value Error.t

  val pp_error : Format.formatter -> error -> unit

  val string_of_error : error -> string

  val of_string : string -> (value, error) result

  val of_file : string -> (value, error) result

  (** The type of decoders.

      Use the functions below to construct decoders for your data types.

      To run a decoder, pass it to {!val:decode_value}.
   *)
  type 'a decoder = (value, 'a) Decoder.t

  (** {2 Decoding values} *)

  val data : string decoder
  (** Decode a [string]. *)

  val tag : string -> unit decoder
  (** Assert the name of the current tag *)

  val any_tag : string decoder

  val attr : string -> string decoder
  (** [attr name] decodes the attribute named [name]. *)

  val attr_opt : string -> string option decoder
  (** [attr_opt name] decodes the attribute named [name], if present. *)

  val attrs : (string * string) list decoder
  (** [attrs] decodes the attributes as an assoc list. *)

  (** {3 Children} *)

  val children : 'a decoder -> 'a list decoder

  val pick_children : 'a decoder decoder -> 'a list decoder

  (** {2 Inconsistent structure} *)

  val value : ('i, 'i) Decoder.t
  (** Decode a literal [value]. *)

  val maybe : ('i, 'a) Decoder.t -> ('i, 'a option) Decoder.t
  (** [maybe d] is a decoder that always succeeds. If [d] succeeds with [x],
      then [maybe d] succeeds with [Some x], otherwise if [d] fails, then [maybe d]
      succeeds with [None].
  *)

  val one_of : ('i, 'a) Decoder.t list -> ('i, 'a) Decoder.t
  (** Try a sequence of different decoders. *)

  val pick : ('i, ('i, 'o) Decoder.t) Decoder.t list -> ('i, 'o) Decoder.t
  (** [pick choices] picks a single choice, like {!one_of}.
      However, each element of [choices] can look at the value, decide if
      it applies (e.g. based on the value of a single field, like a "kind"
      or "type" field), and if it does, returns a decoder for the rest of
      the value.

      If a choice is made, even if the returned sub-decoder fails, the
      error message will totally ignore the rest of the choices and only be
      about the choice that was initially made.
   *)

  val decode_sub : 'i -> ('i, 'o) Decoder.t -> ('i, 'o) Decoder.t
  (** [decode_sub value sub_dec] uses [sub_dec] to decode [value].
      This is useful when one has a value on hand.
   *)

  (** {2 Mapping} *)

  val map : ('a -> 'b) -> ('i, 'a) Decoder.t -> ('i, 'b) Decoder.t
  (** Map over the result of a decoder. *)

  val apply :
    ('i, 'a -> 'b) Decoder.t -> ('i, 'a) Decoder.t -> ('i, 'b) Decoder.t
  (** Try two decoders and then combine the result. We can use this to decode
      objects with many fields (but it's preferable to use [Infix.(>>=)] - see the README).
  *)

  (** {2 Fancy decoding} *)

  val pure : 'a -> ('i, 'a) Decoder.t
  (** A decoder that always succeeds with the argument, ignoring the input. *)

  val succeed : 'a -> ('i, 'a) Decoder.t
  (** Alias for [pure]. *)

  val fail : string -> ('i, 'o) Decoder.t
  (** A decoder that always fails with the given message, ignoring the input. *)

  val fail_with : 'i Error.t -> ('i, 'o) Decoder.t

  val from_result : ('a, 'i Error.t) result -> ('i, 'a) Decoder.t

  val and_then :
    ('a -> ('i, 'b) Decoder.t) -> ('i, 'a) Decoder.t -> ('i, 'b) Decoder.t
  (** Create decoders that depend on previous results. *)

  val fix : (('i, 'o) Decoder.t -> ('i, 'o) Decoder.t) -> ('i, 'o) Decoder.t
  (** Recursive decoders.

      [let my_decoder = fix (fun my_decoder -> ...)] allows you to define
      [my_decoder] in terms of itself.
  *)

  module Infix : sig
    include module type of Decoder.Infix

    val ( <$> ) : ('a -> 'b) -> ('i, 'a) Decoder.t -> ('i, 'b) Decoder.t
  end

  include module type of Infix

  (** {2 Running decoders} *)

  val decode_value : 'a decoder -> value -> ('a, error) result
  (** Run a decoder on some input. *)

  val decode_string : 'a decoder -> string -> ('a, error) result
  (** Run a decoder on a string. *)

  val decode_file : 'a decoder -> string -> ('a, error) result
  (** Run a decoder on a file. *)
end
