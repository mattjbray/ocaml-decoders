(** Functors for creating Decoders. *)

open Util

type ('good, 'bad) result = ('good, 'bad) My_result.t =
  | Ok of 'good
  | Error of 'bad

module type S = Sig.S

module type Decodeable = Sig.Decodeable

module Make (Decodeable : Decodeable) :
  Sig.S
    with type value = Decodeable.value
     and type 'a decoder = (Decodeable.value, 'a) Decoder.t = struct
  type value = Decodeable.value

  let pp = Decodeable.pp

  type error = value Error.t

  let pp_error = Error.pp pp

  let string_of_error = Error.to_string pp

  let of_string : string -> (value, error) result =
   fun string ->
    Decodeable.of_string string
    |> My_result.map_err (fun msg ->
           Error.tag "Json parse error" (Error.make msg) )


  let of_file : string -> (value, error) result =
   fun file ->
    Decodeable.of_file file
    |> My_result.map_err (fun msg ->
           Error.tag (Printf.sprintf "While reading %s" file) (Error.make msg) )


  type 'a decoder = (value, 'a) Decoder.t

  let succeed x = Decoder.pure x

  let fail = Decoder.fail

  let fail_with = Decoder.fail_with

  let from_result = Decoder.of_result

  let value = Decoder.value

  let map = Decoder.map

  let apply = Decoder.apply

  let and_then = Decoder.bind

  let fix = Decoder.fix

  let maybe = Decoder.maybe

  module Infix = struct
    include Decoder.Infix

    let ( <$> ) = map
  end

  let nullable (decoder : 'a decoder) : 'a option decoder =
   fun input ->
    match Decodeable.get_null input with
    | Some () ->
        Ok None
    | None ->
        decoder input
        |> My_result.map My_opt.return
        |> My_result.map_err (Error.tag "Expected null or")


  let one_of (decoders : (string * 'a decoder) list) : 'a decoder =
    let decoders =
      decoders
      |> My_list.map (fun (name, d) ->
             d
             |> Decoder.map_err (fun e ->
                    Error.tag_group (Printf.sprintf "%S decoder" name) [ e ] ) )
    in
    Decoder.one_of decoders
    |> Decoder.map_err
         (Error.tag "I tried the following decoders but they all failed")


  let pick decoders =
    let decoders =
      decoders
      |> My_list.map (fun (name, d) ->
             d
             |> Decoder.map_err (fun e ->
                    Error.tag_group (Printf.sprintf "%S decoder" name) [ e ] ) )
    in
    Decoder.pick decoders
    |> Decoder.map_err
         (Error.tag "I tried the following decoders but they all failed")


  let decode_sub = Decoder.decode_sub

  let primitive_decoder (get_value : value -> 'a option) (message : string) :
      'a decoder =
   fun t ->
    match get_value t with Some value -> Ok value | _ -> (fail message) t


  let string : string decoder =
    Decoder.of_to_opt Decodeable.get_string (fail "Expected a string")


  let int : int decoder = primitive_decoder Decodeable.get_int "Expected an int"

  let float : float decoder =
    primitive_decoder Decodeable.get_float "Expected a float"


  let bool : bool decoder =
    primitive_decoder Decodeable.get_bool "Expected a bool"


  let null : unit decoder =
    primitive_decoder Decodeable.get_null "Expected a null"


  let list : 'a decoder -> 'a list decoder =
   fun decoder t ->
    match Decodeable.get_list t with
    | None ->
        (fail "Expected a list") t
    | Some values ->
        values
        |> My_list.mapi (fun i x ->
               decoder x
               |> My_result.map_err (Error.tag (Printf.sprintf "element %i" i)) )
        |> My_result.combine_l
        |> My_result.map_err (Error.tag_group "while decoding a list")


  let list_filter : 'a option decoder -> 'a list decoder =
   fun decoder ->
    let rec go i = function
      | [] ->
          Ok []
      | v :: vs ->
          My_result.Infix.(
            decoder v
            |> My_result.map_err (Error.tag (Printf.sprintf "element %i" i))
            >>= (function
            | Some x ->
                go (i + 1) vs >>= fun xs -> My_result.return (x :: xs)
            | None ->
                go (i + 1) vs ))
    in
    fun t ->
      match Decodeable.get_list t with
      | None ->
          (fail "Expected a list") t
      | Some values ->
          go 0 values |> My_result.map_err (Error.tag "while decoding a list")


  let list_fold_left : ('a -> 'a decoder) -> 'a -> 'a decoder =
   fun decoder_func init t ->
    match Decodeable.get_list t with
    | None ->
        (fail "Expected a list") t
    | Some values ->
        values
        |> My_result.Infix.(
             My_list.fold_left
               (fun (acc, i) el ->
                 ( ( acc
                   >>= fun acc ->
                   (acc |> decoder_func) el
                   |> My_result.map_err
                        (Error.tag (Printf.sprintf "element %i" i)) )
                 , i + 1 ) )
               (Ok init, 0))
        |> fst
        |> My_result.map_err (Error.tag "while decoding a list")


  let array : 'a decoder -> 'a array decoder =
   fun decoder t ->
    let res = (list decoder) t in
    match res with
    | Ok x ->
        Ok (Array.of_list x)
    | Error e ->
        Error
          (Error.map_tag
             (function
               | "while decoding a list" -> "while decoding an array" | s -> s
               )
             e )


  let field : string -> 'a decoder -> 'a decoder =
   fun key value_decoder t ->
    let value =
      Decodeable.get_key_value_pairs t
      |> My_opt.flat_map
           (My_list.find_map (fun (k, v) ->
                match Decodeable.get_string k with
                | Some s when s = key ->
                    Some v
                | _ ->
                    None ) )
    in
    match value with
    | Some value ->
        value_decoder value
        |> My_result.map_err (Error.tag (Printf.sprintf "in field %S" key))
    | None ->
        (fail (Printf.sprintf "Expected an object with an attribute %S" key)) t


  let field_opt : string -> 'a decoder -> 'a option decoder =
   fun key value_decoder t ->
    let value =
      Decodeable.get_key_value_pairs t
      |> My_opt.flat_map
           (My_list.find_map (fun (k, v) ->
                match Decodeable.get_string k with
                | Some s when s = key ->
                    Some v
                | _ ->
                    None ) )
    in
    match value with
    | Some value ->
        value_decoder value
        |> My_result.map (fun v -> Some v)
        |> My_result.map_err (Error.tag (Printf.sprintf "in field %S" key))
    | None ->
        Ok None


  let field_opt_or : default:'a -> string -> 'a decoder -> 'a decoder =
   fun ~default key value_decoder t ->
    match field_opt key value_decoder t with
    | Ok (Some x) ->
        Ok x
    | Ok None ->
        Ok default
    | Error _ as e ->
        e


  let single_field : (string -> 'a decoder) -> 'a decoder =
   fun value_decoder t ->
    match Decodeable.get_key_value_pairs t with
    | Some [ (key, value) ] ->
      ( match Decodeable.get_string key with
      | Some key ->
          (value_decoder key) value
          |> My_result.map_err (Error.tag (Printf.sprintf "in field %S" key))
      | None ->
          (fail "Expected an object with a string key") t )
    | _ ->
        (fail "Expected an object with a single attribute") t


  let index : int -> 'a decoder -> 'a decoder =
   fun i decoder t ->
    match Decodeable.get_list t with
    | Some l ->
        let item =
          try Some (List.nth l i) with
          | Failure _ ->
              None
          | Invalid_argument _ ->
              None
        in
        ( match item with
        | None ->
            (fail
               ("expected a list with at least " ^ string_of_int i ^ " elements") )
              t
        | Some item ->
            decoder item )
    | None ->
        (fail "Expected a list") t


  let uncons (tail : 'a -> 'b decoder) (head : 'a decoder) : 'b decoder =
   fun value ->
    match Decodeable.get_list value with
    | Some (x :: rest) ->
        My_result.Infix.(
          head x
          |> My_result.map_err (Error.tag "while consuming a list element")
          >>= fun x ->
          (tail x) (Decodeable.to_list rest)
          |> My_result.map_err (Error.tag "after consuming a list element"))
    | Some [] ->
        (fail "Expected a non-empty list") value
    | None ->
        (fail "Expected a list") value


  let rec at : string list -> 'a decoder -> 'a decoder =
   fun path decoder ->
    match path with
    | [ key ] ->
        field key decoder
    | key :: rest ->
        field key (at rest decoder)
    | [] ->
        fail "Must provide at least one key to 'at'"


  let keys' : 'k decoder -> 'k list decoder =
   fun key_decoder value ->
    match Decodeable.get_key_value_pairs value with
    | Some assoc ->
        assoc
        |> List.map (fun (key, _) -> key_decoder key)
        |> My_result.combine_l
        |> My_result.map_err
             (Error.tag_group "Failed while decoding the keys of an object")
    | None ->
        (fail "Expected an object") value


  let keys = keys' string

  let key_value_pairs' : 'k decoder -> 'v decoder -> ('k * 'v) list decoder =
   fun key_decoder value_decoder value ->
    match Decodeable.get_key_value_pairs value with
    | Some assoc ->
        assoc
        |> List.map
             My_result.Infix.(
               fun (key_val, value_val) ->
                 key_decoder key_val
                 >>= fun key ->
                 value_decoder value_val >|= fun value -> (key, value))
        |> My_result.combine_l
        |> My_result.map_err
             (Error.tag_group "Failed while decoding key-value pairs")
    | None ->
        (fail "Expected an object") value


  let key_value_pairs value_decoder = key_value_pairs' string value_decoder

  let key_value_pairs_seq' : 'k decoder -> ('k -> 'v decoder) -> 'v list decoder
      =
   fun key_decoder value_decoder value ->
    match Decodeable.get_key_value_pairs value with
    | Some assoc ->
        assoc
        |> List.map
             My_result.Infix.(
               fun (key_val, value_val) ->
                 key_decoder key_val
                 >>= fun key -> (value_decoder key) value_val)
        |> My_result.combine_l
        |> My_result.map_err
             (Error.tag_group "Failed while decoding key-value pairs")
    | None ->
        (fail "Expected an object") value


  let key_value_pairs_seq value_decoder =
    key_value_pairs_seq' string value_decoder


  let decode_value (decoder : 'a decoder) (input : value) : ('a, error) result =
    decoder input


  let of_of_string ~msg of_string =
    let open Infix in
    string
    >|= of_string
    >>= function Some x -> succeed x | None -> fail ("Expected " ^ msg)


  let decode_string : 'a decoder -> string -> ('a, error) result =
   fun decoder string ->
    My_result.Infix.(of_string string >>= decode_value decoder)


  let decode_file : 'a decoder -> string -> ('a, error) result =
   fun decoder file -> My_result.Infix.(of_file file >>= decode_value decoder)


  module Pipeline = struct
    let decode = succeed

    let custom : 'a decoder -> ('a -> 'b) decoder -> 'b decoder =
     fun customDecoder next -> apply next customDecoder


    let required : string -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder =
     fun key decoder next -> custom (field key decoder) next


    let required_at :
        string list -> 'a decoder -> ('a -> 'b) decoder -> 'b decoder =
     fun path decoder next -> custom (at path decoder) next


    let optional_decoder : value decoder -> 'a decoder -> 'a -> 'a decoder =
     fun path_decoder val_decoder default ->
      let null_or decoder =
        one_of
          [ ("non-null", decoder); ("null", null |> map (fun () -> default)) ]
      in
      let handle_result : value -> 'a decoder =
       fun input ->
        match decode_value path_decoder input with
        | Ok rawValue ->
            (* The field was present. *)
            decode_value (null_or val_decoder) rawValue |> from_result
        | Error _ ->
            (* The field was not present. *)
            succeed default
      in
      value |> and_then handle_result


    let optional :
        string -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder =
     fun key val_decoder default next ->
      custom (optional_decoder (field key value) val_decoder default) next


    let optional_at :
        string list -> 'a decoder -> 'a -> ('a -> 'b) decoder -> 'b decoder =
     fun path val_decoder default next ->
      custom (optional_decoder (at path value) val_decoder default) next
  end

  include Infix
end
