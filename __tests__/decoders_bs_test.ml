open Jest
open Decoders_bs

external parse_int : string -> int = "parseInt" [@@bs.scope "window"] [@@bs.val]

let () =
  describe
    "decoders-bs decode"
    Expect.(
      fun () ->
        test
          "string"
          Decode.(
            fun () ->
              let json_str = {|"Hello world"|} in
              let decoded = decode_string string json_str in
              expect decoded |> toEqual (Belt.Result.Ok "Hello world")))


let () =
  describe
    "decoders-bs decode int"
    Expect.(
      fun () ->
        test
          "int"
          Decode.(
            fun () ->
              let json_str = {|5078476151|} in
              let decoded = decode_string int json_str in
              expect decoded |> toEqual (Belt.Result.Ok (parse_int "5078476151"))))


let () =
  describe
    "decoders-bs decode array"
    Expect.(
      fun () ->
        test
          "array"
          Decode.(
            fun () ->
              let json_str = {|["a", "b", "c"]|} in
              let decoded = decode_string (array string) json_str in
              expect decoded |> toEqual (Belt.Result.Ok [| "a"; "b"; "c" |])))


let () =
  describe
    "decoders-bs decode error"
    Expect.(
      fun () ->
        test
          "array"
          Decode.(
            fun () ->
              let json_str = {|["a", 1, "c"]|} in
              let decoded = decode_string (array string) json_str in
              expect decoded
              |> toEqual
                   (Belt.Result.Error
                      Decoders.Error.(
                        tag_group
                          "while decoding an array"
                          [ tag
                              "element 1"
                              (make
                                 "Expected a string"
                                 ~context:(Js.Json.number 1.) )
                          ]) )))


let () =
  describe
    "decoders-bs encode"
    Expect.(
      fun () ->
        test
          "string"
          Encode.(
            fun () ->
              let str = "Hello world" in
              let encoded = encode_string string str in
              expect encoded |> toEqual {|"Hello world"|}))


let () =
  describe
    "decoders-bs encode array"
    Expect.(
      fun () ->
        test
          "string"
          Encode.(
            fun () ->
              let x = [| "a"; "b"; "c" |] in
              let encoded = encode_string (array string) x in
              expect encoded |> toEqual {|["a","b","c"]|}))
