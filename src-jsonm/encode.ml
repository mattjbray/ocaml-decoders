module L = Decoders.Decoders_util.My_list 

module Jsonm_encodeable = struct
  type value = Jsonm.lexeme list

  let to_string (v : value) : string =
    let buffer = Buffer.create 16 in
    let dst = `Buffer buffer in
    let encoder = Jsonm.encoder ~minify:true dst in
    let encode = Jsonm.encode encoder in
    v |> List.iter  (fun lexeme ->
        let _ = encode (`Lexeme lexeme) in
        ()
      );
    let _ = encode `End in
    Buffer.contents buffer

  let of_string x : value = [ `String x ]

  let of_int x : value = [ `Float (float_of_int x) ]

  let of_float x : value = [ `Float x ]

  let of_bool x : value = [ `Bool x ]

  let null : value = [`Null]

  let of_list (xs : value list) : value =
    `As :: (List.concat xs) @ [ `Ae ]

  let of_key_value_pairs (xs : (value * value) list) : value =
    `Os ::
    (xs |> L.flat_map (fun (k, v) ->
        match k with
        | [`String k] ->  `Name k :: v
        | _ -> []))
    @ [`Oe]
end

include Decoders.Encode.Make(Jsonm_encodeable)
