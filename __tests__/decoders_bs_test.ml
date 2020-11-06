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
