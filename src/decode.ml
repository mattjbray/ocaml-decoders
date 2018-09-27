(** Functors for creating Decoders. *)

open Util

type 'value exposed_error =
  | Decoder_error of string * 'value option
  | Decoder_errors of 'value exposed_error list
  | Decoder_tag of string * 'value exposed_error

type ('good, 'bad) result = ('good, 'bad) My_result.t = Ok of 'good | Error of 'bad

type ('value, 'a) exposed_decoder = { run : 'value -> ('a, 'value exposed_error) result }

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
end

(** User-facing Decoder interface. *)
module type S = sig
  (** The type of values to be decoded (e.g. JSON or Yaml). *)
  type value

  type error = value exposed_error

  val pp_error : Format.formatter -> error -> unit

  val of_string : string -> (value, error) result
  val of_file : string -> (value, error) result

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

  (** Decode a literal [value]. *)
  val value : value decoder

  (** {1 Data structures} *)

  (** Decode a collection into an OCaml list. *)
  val list : 'a decoder -> 'a list decoder

  val list_filter : 'a option decoder -> 'a list decoder

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

  val keys : string list decoder
  val key_value_pairs : 'v decoder -> (string * 'v) list decoder
  val key_value_pairs_seq : (string -> 'v decoder) -> 'v list decoder

  val keys' : 'k decoder -> 'k list decoder
  val key_value_pairs' : 'k decoder -> 'v decoder -> ('k * 'v) list decoder
  val key_value_pairs_seq' : 'k decoder -> ('k -> 'v decoder) -> 'v list decoder

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

  include module type of Infix

  (** {1 Running decoders} *)

  (** Run a decoder on some input. *)
  val decode_value : 'a decoder -> value -> ('a, error) result

  (** Run a decoder on a string. *)
  val decode_string : 'a decoder -> string -> ('a, error) result

  (** Run a decoder on a file. *)
  val decode_file : 'a decoder -> string -> ('a, error) result

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
    val required_at : string list -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder
    val optional : string -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder
    val optional_at : string list -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder
    val custom : 'a decoder -> ('a -> 'b) decoder -> 'b decoder
  end
end

module Make(Decodeable : Decodeable) : S with type value = Decodeable.value
                                          and type 'a decoder = (Decodeable.value, 'a) exposed_decoder
= struct
  type value = Decodeable.value
  let pp = Decodeable.pp

  type error = value exposed_error

  let rec pp_error fmt = function
    | Decoder_error (msg, Some t) -> Format.fprintf fmt "@[%s, but got@ @[%a@]@]" msg pp t
    | Decoder_error (msg, None) -> Format.fprintf fmt "@[%s@]" msg
    | Decoder_errors errors ->
      let errors_trunc = My_list.take 5 errors in
      let not_shown = List.length errors - 5 in
      Format.fprintf fmt "@[%a@ %s@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_error) errors_trunc
        (if not_shown > 0 then Printf.sprintf "(...%d errors not shown...)" not_shown else "")
    | Decoder_tag (msg, error) ->
      Format.fprintf fmt "@[<2>%s:@ @[%a@]@]"
        msg
        pp_error error

  let tag_error (msg : string) (error : error) : error =
    Decoder_tag (msg, error)

  let tag_errors (msg : string) (errors : error list) : error =
    Decoder_tag (msg, Decoder_errors errors)

  let merge_errors e1 e2 =
    match e1, e2 with
    | Decoder_errors e1s, Decoder_errors e2s -> Decoder_errors (e1s @ e2s)
    | Decoder_errors e1s, _ -> Decoder_errors (e1s @ [e2])
    | _, Decoder_errors e2s -> Decoder_errors ([e1] @ e2s )
    | _ -> Decoder_errors [e1; e2]

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

  let of_string : string -> (value, error) result =
    fun string ->
      Decodeable.of_string string
      |> My_result.map_err (fun msg ->
          (Decoder_tag ("Json parse error", Decoder_error (msg, None)))
        )

  let of_file : string -> (value, error) result =
    fun file ->
      Decodeable.of_file file
      |> My_result.map_err (fun msg ->
          (Decoder_tag (Printf.sprintf "While reading %s" file, Decoder_error (msg, None)))
        )

  type 'a decoder = (value, 'a) exposed_decoder

  let succeed x =
    { run = fun _ -> Ok x }

  let fail msg =
    { run = fun input -> Error (Decoder_error (msg, Some input)) }

  let fail_with error =
    { run = fun _ -> Error error }

  let from_result = function
    | Ok ok -> succeed ok
    | Error error -> fail_with error

  let value =
    { run = fun input -> Ok input }

  let map f decoder =
    { run = fun input -> My_result.Infix.(decoder.run input >|= f) }

  let apply : ('a -> 'b) decoder -> 'a decoder -> 'b decoder =
    fun f decoder ->
      { run =
          fun input ->
            match f.run input, decoder.run input with
            | Error e1, Error e2 -> Error (merge_errors e1 e2)
            | Error e, _ -> Error e
            | _, Error e -> Error e
            | Ok g, Ok x -> Ok (g x)
      }

  let and_then (f : 'a -> 'b decoder) (decoder : 'a decoder) : 'b decoder =
    { run = fun input ->
          My_result.Infix.(
            decoder.run input >>= fun result ->
            (f result).run input)
    }

  let fix (f : 'a decoder -> 'a decoder) : 'a decoder =
    let rec p = lazy (f r)
    and r = { run = fun value -> (Lazy.force p).run value }
    in
    r

  module Infix = struct
    let (>|=) x f = map f x
    let (>>=) x f = and_then f x
    let (<*>) f x = apply f x
  end

  let maybe (decoder : 'a decoder) : 'a option decoder =
    { run = fun input ->
      match (decoder.run input) with
      | Ok result -> Ok (Some result)
      | Error _ -> Ok None
    }

  let nullable (decoder : 'a decoder) : 'a option decoder =
    { run =
        fun input ->
          match Decodeable.get_null input with
          | Some () -> Ok None
          | None ->
            decoder.run input
            |> My_result.map My_opt.return
            |> My_result.map_err (tag_error "Expected null or")
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

  let primitive_decoder (get_value : value -> 'a option) (message : string) : 'a decoder =
    { run =
        fun t ->
          match get_value t with
          | Some value -> Ok value
          | _ -> (fail message).run t
    }

  let string : string decoder =
    primitive_decoder Decodeable.get_string "Expected a string"

  let int : int decoder =
    primitive_decoder Decodeable.get_int "Expected an int"

  let float : float decoder =
    primitive_decoder Decodeable.get_float "Expected a float"

  let bool : bool decoder =
    primitive_decoder Decodeable.get_bool "Expected a bool"

  let null : 'a -> 'a decoder =
    fun default ->
      primitive_decoder Decodeable.get_null "Expected a null"
      |> map (fun _ -> default)

  let list : 'a decoder -> 'a list decoder =
    fun decoder ->
      { run =
          fun t ->
            match Decodeable.get_list t with
            | None -> (fail "Expected a list").run t
            | Some values ->
              values
              |> My_list.mapi (fun i x ->
                  decoder.run x
                  |> My_result.map_err
                    (tag_error (Printf.sprintf "element %i" i))
                )
              |> combine_errors
              |> My_result.map_err
                (tag_errors "while decoding a list")
      }

  let list_filter : 'a option decoder -> 'a list decoder =
    fun decoder ->
      let rec go i = function
        | [] -> Ok []
        | v :: vs ->
         My_result.Infix.(
           decoder.run v
           |> My_result.map_err
             (tag_error (Printf.sprintf "element %i" i)) >>= function
           | Some x ->
             go (i + 1) vs >>= fun xs ->
             My_result.return (x :: xs)
           | None -> go (i + 1) vs
         )
      in
      { run =
          fun t ->
            match Decodeable.get_list t with
            | None -> (fail "Expected a list").run t
            | Some values ->
              go 0 values
              |> My_result.map_err (tag_error "while decoding a list")
      }

  let field : string -> 'a decoder -> 'a decoder =
    fun key value_decoder ->
      { run =
          fun t ->
            let value =
              Decodeable.get_key_value_pairs t
              |> My_opt.flat_map (My_list.find_map (fun (k, v) ->
                  match Decodeable.get_string k with
                  | Some s when s = key -> Some v
                  | _ -> None
                ))
            in
            match value with
            | Some value ->
              value_decoder.run value
              |> My_result.map_err (tag_error (Printf.sprintf "in field %S" key))
            | None -> (fail (Printf.sprintf "Expected an object with an attribute %S" key)).run t
      }

  let single_field : (string -> 'a decoder) -> 'a decoder =
    fun value_decoder ->
      { run =
          fun t ->
            match Decodeable.get_key_value_pairs t with
            | Some [(key, value)] ->
              begin match Decodeable.get_string key with
                | Some key ->
                  (value_decoder key).run value
                  |> My_result.map_err (tag_error (Printf.sprintf "in field %S" key))
                | None -> (fail "Expected an object with a string key").run t
              end
            | _ -> (fail "Expected an object with a single attribute").run t
      }

  let index : int -> 'a decoder -> 'a decoder =
    fun i decoder ->
      { run =
          fun t ->
            match Decodeable.get_list t with
            | Some l ->
              let item =
                try Some (List.nth l i) with
                | Failure _-> None
                | Invalid_argument _ -> None
              in
              begin match item with
                | None -> (fail ("expected a list with at least " ^ string_of_int i ^ " elements")).run t
                | Some item -> decoder.run item
              end
            | None -> (fail "Expected a list").run t
      }

  let rec at : string list -> 'a decoder -> 'a decoder = fun path decoder ->
    match path with
    | [key] -> field key decoder
    | key :: rest -> field key (at rest decoder)
    | [] -> fail "Must provide at least one key to 'at'"

  let keys' : 'k decoder -> 'k list decoder =
    fun key_decoder ->
      { run =
          fun value ->
            match Decodeable.get_key_value_pairs value with
            | Some assoc ->
              assoc
              |> List.map (fun (key, _) -> key_decoder.run key)
              |> combine_errors
              |> My_result.map_err
                (tag_errors "Failed while decoding the keys of an object")
            | None -> (fail "Expected an object").run value
      }

  let keys = keys' string

  let key_value_pairs' : 'k decoder -> 'v decoder -> ('k * 'v) list decoder =
    fun key_decoder value_decoder ->
      { run =
          fun value ->
            match Decodeable.get_key_value_pairs value with
            | Some assoc ->
              assoc
              |> List.map
                My_result.Infix.(fun (key_val, value_val) ->
                    key_decoder.run key_val >>= fun key ->
                    value_decoder.run value_val >|= fun value ->
                    (key, value)
                  )
              |> combine_errors
              |> My_result.map_err
                (tag_errors "Failed while decoding key-value pairs")
            | None -> (fail "Expected an object").run value
      }

  let key_value_pairs value_decoder = key_value_pairs' string value_decoder

  let key_value_pairs_seq' : 'k decoder -> ('k -> 'v decoder) -> 'v list decoder =
    fun key_decoder value_decoder ->
      { run =
          fun value ->
            match Decodeable.get_key_value_pairs value with
            | Some assoc ->
              assoc
              |> List.map
                My_result.Infix.(fun (key_val, value_val) ->
                    key_decoder.run key_val >>= fun key ->
                    (value_decoder key).run value_val
                  )
              |> combine_errors
              |> My_result.map_err
                (tag_errors "Failed while decoding key-value pairs")
            | None -> (fail "Expected an object").run value
      }

  let key_value_pairs_seq value_decoder = key_value_pairs_seq' string value_decoder

  let decode_value (decoder : 'a decoder) (input : value) : ('a, error) result =
      decoder.run input

  let decode_string : 'a decoder -> string -> ('a, error) result =
    fun decoder string ->
      My_result.Infix.(of_string string >>= decode_value decoder)

  let decode_file : 'a decoder -> string -> ('a, error) result =
    fun decoder file ->
      My_result.Infix.(of_file file >>= decode_value decoder)

  module Pipeline = struct
    let decode = succeed

    let custom : 'a decoder -> ('a -> 'b) decoder -> 'b decoder =
      fun customDecoder next ->
        apply next customDecoder

    let required : string -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder =
      fun key decoder next ->
        custom (field key decoder) next

    let required_at : string list -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder =
      fun path decoder next ->
        custom (at path decoder) next

    let optional_decoder : value decoder -> 'a decoder -> 'a -> 'a decoder =
      fun path_decoder val_decoder default ->
        let null_or decoder =
          one_of
            [ ( "non-null", decoder )
            ; ( "null", null default )
            ] in
        let handle_result : value -> 'a decoder =
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

    let optional_at : string list -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder =
      fun path val_decoder default next ->
        custom (optional_decoder (at path value) val_decoder default) next
  end

  include Infix
end
