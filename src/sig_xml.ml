(** Interface for decoding XML *)
module type S = sig
  (** The type of XML values. *)
  type value

  type tag

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

  type 'a tag_decoder = (tag, 'a) Decoder.t

  (** {2 Primitives} *)

  val tag : string -> 'a tag_decoder -> 'a decoder

  val any_tag : (string -> 'a tag_decoder) -> 'a decoder

  val data : string decoder
  (** Decode a [string]. *)

  val value : value decoder
  (** Decode a literal [value]. *)

  (** {2 Decoding the contents of a tag}*)

  val attr : string -> string tag_decoder
  (** [attr name] decodes the attribute named [name]. *)

  val attr_opt : string -> string option tag_decoder
  (** [attr_opt name] decodes the attribute named [name], if present. *)

  val attrs : (string * string) list tag_decoder
  (** [attrs] decodes the attributes as an assoc list. *)

  val children : 'a decoder -> 'a list tag_decoder

  val single_child : 'a decoder -> 'a tag_decoder

  val first_child : 'a decoder -> 'a tag_decoder

  (** {2 Inconsistent structure} *)

  val maybe : 'a decoder -> 'a option decoder
  (** [maybe d] is a decoder that always succeeds. If [d] succeeds with [x],
      then [maybe d] succeeds with [Some x], otherwise if [d] fails, then [maybe d]
      succeeds with [None].
  *)

  val one_of : (string * 'a decoder) list -> 'a decoder
  (** Try a sequence of different decoders. *)

  val pick : (string * 'a decoder decoder) list -> 'a decoder
  (** [pick choices] picks a single choice, like {!one_of}.
      However, each element of [choices] can look at the value, decide if
      it applies (e.g. based on the value of a single field, like a "kind"
      or "type" field), and if it does, returns a decoder for the rest of
      the value.

      If a choice is made, even if the returned sub-decoder fails, the
      error message will totally ignore the rest of the choices and only be
      about the choice that was initially made.
   *)

  val decode_sub : value -> 'a decoder -> 'a decoder
  (** [decode_sub value sub_dec] uses [sub_dec] to decode [value].
      This is useful when one has a value on hand.
   *)

  (** {2 Mapping} *)

  val map : ('a -> 'b) -> 'a decoder -> 'b decoder
  (** Map over the result of a decoder. *)

  val apply : ('a -> 'b) decoder -> 'a decoder -> 'b decoder
  (** Try two decoders and then combine the result. We can use this to decode
      objects with many fields (but it's preferable to use [Infix.(>>=)] - see the README).
  *)

  (** {2 Fancy decoding} *)

  val succeed : 'a -> 'a decoder
  (** A decoder that always succeeds with the argument, ignoring the input. *)

  val fail : string -> 'a decoder
  (** A decoder that always fails with the given message, ignoring the input. *)

  val fail_with : error -> 'a decoder

  val from_result : ('a, error) result -> 'a decoder

  val and_then : ('a -> 'b decoder) -> 'a decoder -> 'b decoder
  (** Create decoders that depend on previous results. *)

  val fix : ('a decoder -> 'a decoder) -> 'a decoder
  (** Recursive decoders.

      [let my_decoder = fix (fun my_decoder -> ...)] allows you to define
      [my_decoder] in terms of itself.
  *)

  val of_of_string : msg:string -> (string -> 'a option) -> 'a decoder
  (** Create a decoder from a function [of_string : string -> 'a option] *)

  module Infix : sig
    include module type of Decoder.Infix

    val ( <$> ) : ('a -> 'b) -> 'a decoder -> 'b decoder
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
