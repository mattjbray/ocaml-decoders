open Decoders_util

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

module Make(E : Encodeable) : S with type value = E.value = struct
  type value = E.value
  type 'a encoder = 'a -> value

  let string x = E.of_string x
  let int x = E.of_int x
  let float x = E.of_float x
  let bool x = E.of_bool x
  let null = E.null

  let option encoder =
    function
    | None -> E.null
    | Some x -> encoder x

  let list encoder xs =
    xs
    |> My_list.map (fun x -> encoder x)
    |> E.of_list

  let obj' xs = E.of_key_value_pairs xs

  let obj xs =
    xs
    |> List.map (fun (k, v) -> (E.of_string k, v))
    |> E.of_key_value_pairs

  let value x = x

  let encode_value encoder x = encoder x
  let encode_string encoder x = encoder x |> E.to_string
end
