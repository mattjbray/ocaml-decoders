open Decoders
open Util

type value = Ezxmlm.node

let pp fmt v = Ezxmlm.pp fmt [ v ]

type error = value Error.t

let pp_error = Error.pp pp

let string_of_error = Error.to_string pp

let pp_name fmt (ns, name) = Format.fprintf fmt "(%S, %S)" ns name

let of_dtd_nodes = function
  | _dtd, [ node ] ->
      Ok node
  | _ ->
      Error (Error.make "expected an XML document with a single root node")


let try_parse_with f x =
  match f x with
  | dtd, nodes ->
      of_dtd_nodes (dtd, nodes)
  | exception e ->
      Error
        (Error.tag
           "Could not parse an XML document"
           (Error.make (Printexc.to_string e)) )


let of_string (s : string) = try_parse_with Ezxmlm.from_string s

let of_channel (ic : in_channel) = try_parse_with Ezxmlm.from_channel ic

let of_file (file : string) =
  try Util.with_file_in file of_channel with
  | e ->
      Error
        (Error.tag "could not open file" (Error.make (Printexc.to_string e)))


type 'a decoder = (value, 'a) Decoder.t

include Decoder
include Decoder.Infix

let succeed = pure

let and_then = bind

let from_result = of_result

let tag_ns (name : Xmlm.name) : unit decoder =
 Decoder.of_decode_fun @@ fun (v : value) ->
  match v with
  | `El ((name', _), _) when name = name' ->
      Ok ()
  | `El _ ->
      Error
        (Error.make
           (Format.asprintf "Expected a tag with name %a" pp_name name)
           ~context:v )
  | `Data _ ->
      (fail "Expected a Tag").dec v


let tag (name : string) : unit decoder =
 Decoder.of_decode_fun @@ fun (v : value) ->
  match v with
  | `El (((_ns, name'), _), _) when name = name' ->
      Ok ()
  | `El _ ->
      Error
        (Error.make
           (Format.asprintf "Expected a tag with name %S" name)
           ~context:v )
  | `Data _ ->
      (fail "Expected a Tag").dec v


let any_tag_ns : Xmlm.name decoder =
 Decoder.of_decode_fun @@ fun (v : value) ->
  match v with
  | `El ((name, _), _) ->
      Ok name
  | `Data _ ->
      (fail "Expected a Tag").dec v


let any_tag : string decoder =
 Decoder.of_decode_fun @@ fun (v : value) ->
  match v with
  | `El (((_ns, name), _), _) ->
      Ok name
  | `Data _ ->
      (fail "Expected a Tag").dec v


let data : string decoder =
 Decoder.of_decode_fun @@ fun (v : value) ->
  match v with `Data s -> Ok s | `El _ -> (fail "Expected Data").dec v


let attrs_ns : Xmlm.attribute list decoder = Decoder.of_decode_fun @@ function
  | `El ((_tag, attrs), _children) ->
      Ok attrs
  | `Data _ as v ->
      (fail "Expected a Tag").dec v


let attr_opt_ns (name : Xmlm.name) : string option decoder =
  attrs_ns
  >|= My_list.find_map (fun (name', value) ->
          if name = name' then Some value else None )


let attr_ns (name : Xmlm.name) : string decoder =
  attr_opt_ns name
  >>= function
  | Some value ->
      pure value
  | None ->
      fail (Format.asprintf "Expected an attribute named %a" pp_name name)


let attrs : (string * string) list decoder =
  attrs_ns >|= My_list.map (fun ((_ns, name), value) -> (name, value))


let attr_opt (name : string) : string option decoder =
  attrs
  >|= My_list.find_map (fun (name', value) ->
          if name = name' then Some value else None )


let attr (name : string) : string decoder =
  attr_opt name
  >>= function
  | Some value ->
      pure value
  | None ->
      fail (Format.asprintf "Expected an attribute named %s" name)


let pick_children (child : 'a decoder decoder) : 'a list decoder = Decoder.of_decode_fun @@ function
  | `El ((name, _attrs), els) ->
      els
      |> My_list.filter_mapi (fun i el ->
             match child.dec el with
             | Error _ ->
                 None
             | Ok dec ->
                 Some
                   ( dec.dec el
                   |> My_result.map_err
                        (Error.tag
                           (Format.asprintf "While decoding child %i" i) ) ) )
      |> My_result.combine_l
      |> My_result.map_err
           (Error.tag_group (Format.asprintf "In tag %a" pp_name name))
  | `Data _ as v ->
      (
      fail "Expected a Tag").dec v


let children (child : 'a decoder) : 'a list decoder = pick_children (pure child)

let decode_value decoder v = decoder.dec v

let decode_string : 'a decoder -> string -> ('a, error) result =
 fun decoder string ->
  My_result.Infix.(of_string string >>= decode_value decoder)


let decode_file : 'a decoder -> string -> ('a, error) result =
 fun decoder file -> My_result.Infix.(of_file file >>= decode_value decoder)
