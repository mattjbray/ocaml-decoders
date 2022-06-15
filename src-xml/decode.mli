type value = Ezxmlm.node

type error = value Decoders.Error.t

val pp_error : Format.formatter -> error -> unit

val string_of_error : error -> string

val of_string : string -> (value, error) result

val of_file : string -> (value, error) result

type 'a decoder = (value, 'a) Decoders.Decoder.t
