(** Functors for creating Decoders. *)

open Decoders_util

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

  val to_list : value list -> value
end

(** User-facing Decoder interface. *)
module type S = sig
  type value
  type error = value exposed_error
  val pp_error : Format.formatter -> error -> unit
  val string_of_error : error -> string

  val of_string : string -> (value, error) result
  val of_file : string -> (value, error) result

  type 'a decoder

  val string : string decoder
  val int : int decoder
  val float : float decoder
  val bool : bool decoder
  val null : unit decoder
  val value : value decoder
  val list : 'a decoder -> 'a list decoder
  val list_filter : 'a option decoder -> 'a list decoder
  val list_fold_left: ('a -> 'a decoder) -> 'a -> 'a decoder
  val index : int -> 'a decoder -> 'a decoder
  val uncons : ('a -> 'b decoder) -> 'a decoder -> 'b decoder
  val field : string -> 'a decoder -> 'a decoder
  val field_opt : string -> 'a decoder -> 'a option decoder
  val single_field : (string -> 'a decoder) -> 'a decoder
  val at : string list -> 'a decoder -> 'a decoder
  val maybe : 'a decoder -> 'a option decoder
  val nullable : 'a decoder -> 'a option decoder
  val one_of : (string * 'a decoder) list -> 'a decoder
  val map : ('a -> 'b) -> 'a decoder -> 'b decoder
  val apply : ('a -> 'b) decoder -> 'a decoder -> 'b decoder
  val keys : string list decoder
  val key_value_pairs : 'v decoder -> (string * 'v) list decoder
  val key_value_pairs_seq : (string -> 'v decoder) -> 'v list decoder
  val keys' : 'k decoder -> 'k list decoder
  val key_value_pairs' : 'k decoder -> 'v decoder -> ('k * 'v) list decoder
  val key_value_pairs_seq' : 'k decoder -> ('k -> 'v decoder) -> 'v list decoder
  val succeed : 'a -> 'a decoder
  val fail : string -> 'a decoder
  val fail_with : error -> 'a decoder
  val from_result : ('a, error) result -> 'a decoder
  val and_then : ('a -> 'b decoder) -> 'a decoder -> 'b decoder
  val fix : ('a decoder -> 'a decoder) -> 'a decoder

  module Infix : sig
    val (>|=) : 'a decoder -> ('a -> 'b) -> 'b decoder
    val (>>=) : 'a decoder -> ('a -> 'b decoder) -> 'b decoder
    val (<*>) : ('a -> 'b) decoder -> 'a decoder -> 'b decoder
  end

  include module type of Infix

  val decode_value : 'a decoder -> value -> ('a, error) result
  val decode_string : 'a decoder -> string -> ('a, error) result
  val decode_file : 'a decoder -> string -> ('a, error) result

  module Pipeline : sig
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

  let string_of_error e : string =
    Format.asprintf "@[<2>%a@?@]" pp_error e

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

  let combine_errors (results : ('a, error) result list) : ('a list, error list) result =
    let rec aux combined =
      function
      | [] ->
        (match combined with
         | Ok xs -> Ok (List.rev xs)
         | Error es -> Error (List.rev es))
      | result :: rest ->
        let combined =
          match result, combined with
          | Ok x, Ok xs -> Ok (x :: xs)
          | Error e, Error es -> Error (e :: es)
          | Error e, Ok _ -> Error [e]
          | Ok _, Error es -> Error es
        in
        aux combined rest
    in
    aux (Ok []) results

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

  let null : unit decoder =
    primitive_decoder Decodeable.get_null "Expected a null"

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

  let list_fold_left : ('a -> 'a decoder) -> 'a -> 'a decoder =
    fun decoder_func init ->
    {
      run =
        fun t ->
          match Decodeable.get_list t with
          | None -> (fail "Expected a list").run t
          | Some values ->
            values
            |> My_result.Infix.(My_list.fold_left (fun (acc,i) el ->
                (acc >>= fun acc ->
                 (acc |> decoder_func).run el
                 |> My_result.map_err (tag_error (Printf.sprintf "element %i" i))),i+1
              )  (Ok init,0)) |> fst
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

  let field_opt : string -> 'a decoder -> 'a option decoder =
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
            |> My_result.map (fun v -> Some v)
            |> My_result.map_err (tag_error (Printf.sprintf "in field %S" key))
          | None -> Ok None
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

  let uncons (tail : 'a -> 'b decoder) (head : 'a decoder) : 'b decoder =
    { run =
        fun value ->
          match Decodeable.get_list value with
        | Some (x :: rest) ->
          My_result.Infix.(
            head.run x
            |> My_result.map_err (tag_error "while consuming a list element")
            >>= fun x ->
            (tail x).run (Decodeable.to_list rest)
            |> My_result.map_err (tag_error "after consuming a list element")
          )
        | Some [] -> (fail "Expected a non-empty list").run value
        | None -> (fail "Expected a list").run value
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
          ; ( "null", null |> map (fun () -> default) )
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
