open Jest
open Decoders_bs

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
