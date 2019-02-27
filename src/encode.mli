module type S = sig
  type value

  type 'a encoder = 'a -> value

  val string : string encoder
  val int : int encoder
  val float : float encoder
  val bool : bool encoder
  val null : value

  val option : 'a encoder -> 'a option encoder

  val list : 'a encoder -> 'a list encoder
  val obj : (string * value) list encoder
  val obj' : (value * value) list encoder

  (** Helpful for dealing with optional fields, e.g:

      type my_obj = { always_there : string; sometimes_there : string option }

      let my_obj (x : my_obj) =
        obj ([("always_there", string x.always_there)]
             |> kv_opt ("sometimes_there", string) x.sometimes_there)
  *)
  val kv_opt : (string * 'a encoder) -> 'a option -> (string * value) list -> (string * value) list

  val value : value encoder

  val encode_value : 'a encoder -> 'a -> value
  val encode_string : 'a encoder -> 'a -> string
end

module type Encodeable = sig
  type value

  val to_string : value -> string

  val of_string : string -> value
  val of_int : int -> value
  val of_float : float -> value
  val of_bool : bool -> value
  val null : value

  val of_list : value list -> value
  val of_key_value_pairs : (value * value) list -> value
end

module Make(E : Encodeable) : S with type value = E.value
