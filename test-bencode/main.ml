open OUnit2

let encode_s = Bencode.encode_to_string

let decoders_suite =
  let open Decoders_bencode.Decode in

  let decoder_test ~decoder ~input ~expected _test_ctxt =
    match decode_string decoder input with
    | Ok value -> assert_equal value expected
    | Error error -> assert_string (Format.asprintf "%a" pp_error error)
  in

  "decoders" >:::
  [ "list string" >::
    decoder_test
      ~decoder:(list string)
      ~input:(encode_s (Bencode.List [Bencode.String "hello"; Bencode.String "world"]))
      ~expected:["hello"; "world"]
  ; "field_opt present" >::
    decoder_test
      ~decoder:(field_opt "optional" string)
      ~input:(encode_s (Bencode.Dict [("optional", Bencode.String "hello")]))
      ~expected:(Some "hello")
  ; "field_opt missing" >::
    decoder_test
      ~decoder:(field_opt "optional" string)
      ~input:(encode_s (Bencode.Dict [("missing", Bencode.String "hello")]))
      ~expected:None
  ; "field_opt decode error" >::
    fun _ ->
      match
        decode_string
          (field_opt "optional" string)
          (encode_s (Bencode.Dict [("optional", Bencode.Integer 123L)]))
      with
      | Ok _ ->
        assert_string
          "expected decode error"
      | Error e ->
        assert_equal ~printer:CCFun.id
          {|in field "optional": Expected a string, but got 123|}
          (Format.asprintf "%a" pp_error e)
  ]

let encoders_suite =
  let open Decoders_bencode.Encode in
  "encoders" >:::
  [ "list string" >::
    (fun _ctxt ->
       assert_equal ~printer:CCFun.id
         (encode_s (Bencode.List [Bencode.String "hello"; Bencode.String "world"]))
         (encode_string (list string) ["hello"; "world"])
    )
  ; "string" >::
    (fun _ctxt ->
       assert_equal ~printer:CCFun.id
         (encode_s (Bencode.String "hello"))
         (encode_string string "hello")
    )
  ]

let () =
  "Bencode" >:::
  [ decoders_suite
  ; encoders_suite
  ]
  |> run_test_tt_main
