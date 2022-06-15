open Decoders

type value = Ezxmlm.node

let pp fmt v = Ezxmlm.pp fmt [ v ]

type error = value Error.t

let pp_error = Error.pp pp

let string_of_error = Error.to_string pp

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
  try Decoders_util.with_file_in file of_channel with
  | e ->
      Error
        (Error.tag "could not open file" (Error.make (Printexc.to_string e)))


type 'a decoder = (value, 'a) Decoder.t
