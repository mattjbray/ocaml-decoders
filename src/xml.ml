(** Interface for decoding XML *)
module type Decode = sig
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

  val float : float decoder
  (** Decode a [float] in a data node after stripping whitespace. The advantage of using [float_decoder] is that it automatically takes care of trimming whitespace, 
   which is useful since a float is not whitespace sensitive.  *)

  val int : int decoder
  (** Decode an [int] in a data node after stripping whitespace. *)

  val bool : bool decoder
  (** Decode a [bool] in a data node after stripping whitespace.  *)

  val tag : string -> unit decoder
  (** Assert the name of the current tag. *)

  val any_tag : string decoder
  (** Retrieve the name of the current tag. *)

  val attr : string -> string decoder
  (** [attr name] decodes the attribute named [name]. *)

  val attr_opt : string -> string option decoder
  (** [attr_opt name] decodes the attribute named [name], if present. *)

  val attrs : (string * string) list decoder
  (** [attrs] decodes the attributes as an assoc list. *)

  (** {3 Children} *)

  val children : 'a decoder -> 'a list decoder
  (** [children dec] Decodes all the children of the current tag using [dec]. *)

  val pick_children : 'a decoder decoder -> 'a list decoder
  (** [pick_children outer_decoder] Decodes the children of the current tag.

      If [outer_decoder] fails, the child is skipped and the error is ignored.

      If [outer_decoder] succeeds with [inner_decoder], [inner_decoder] is used
      to decode the child.

      For example, to decode only children like [<Child>]:

      {[pick_children (tag "Child" >>= fun () -> succeed decode_child)]}

      To decode only child elements, skipping data nodes (often useful when
      there are data nodes introduced by whitespace between elements in the
      XML):

      {[pick_children (any_tag >>= fun tag_name -> succeed (decode_child_tag tag_name))]}
   *)

  (** {2 Inconsistent structure} *)

  val value : value decoder
  (** Decode a literal [value]. *)

  val maybe : 'a decoder -> 'a option decoder
  (** [maybe d] is a decoder that always succeeds. If [d] succeeds with [x],
      then [maybe d] succeeds with [Some x], otherwise if [d] fails, then [maybe d]
      succeeds with [None].
  *)

  val one_of : 'a decoder list -> 'a decoder
  (** Try a sequence of different decoders. *)

  val pick : 'a decoder decoder list -> 'a decoder
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

  val pure : 'a -> 'a decoder
  (** A decoder that always succeeds with the argument, ignoring the input. *)

  val succeed : 'a -> 'a decoder
  (** Alias for [pure]. *)

  val fail : string -> 'a decoder
  (** A decoder that always fails with the given message, ignoring the input. *)

  val fail_with : error -> 'a decoder
  (** A decoder that always fails with the given error, ignoring the input. *)

  val from_result : ('a, error) result -> 'a decoder

  val and_then : ('a -> 'b decoder) -> 'a decoder -> 'b decoder
  (** Create decoders that depend on previous results. *)

  val fix : ('a decoder -> 'a decoder) -> 'a decoder
  (** Recursive decoders.

      [let my_decoder = fix (fun my_decoder -> ...)] allows you to define
      [my_decoder] in terms of itself.
  *)

  module Infix : module type of Decoder.Infix

  include module type of Infix

  (** {2 Running decoders} *)

  val decode_value : 'a decoder -> value -> ('a, error) result
  (** Run a decoder on some input. *)

  val decode_string : 'a decoder -> string -> ('a, error) result
  (** Run a decoder on a string. *)

  val decode_file : 'a decoder -> string -> ('a, error) result
  (** Run a decoder on a file. *)
end

module type Encode = sig
  type value

  type 'a encoder = 'a -> value

  val tag : string -> ?attrs:(string * string) list -> value list -> value

  val data : string encoder

  val value : value encoder

  val encode_string : 'a encoder -> 'a -> string
end
