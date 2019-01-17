open OUnit2

type tree = Leaf of int | Node of tree * tree

let ezjsonm_suite =
  let open Decoders_ezjsonm.Decode in

  let decoder_test ~decoder ~input ~expected _test_ctxt =
    match decode_string decoder input with
    | Ok value -> assert_equal value expected
    | Error error -> assert_string (Format.asprintf "%a" pp_error error)
  in

  "Ezjsonm" >:::
  [ "list string" >::
    decoder_test
      ~decoder:(list string)
      ~input:{|["hello", "world"]|}
      ~expected:["hello"; "world"]
  ; "field_opt present" >::
    decoder_test
      ~decoder:(field_opt "optional" string)
      ~input:{|{"optional": "hello"}|}
      ~expected:(Some "hello")
  ; "field_opt missing" >::
    decoder_test
      ~decoder:(field_opt "optional" string)
      ~input:{|{"missing": "hello"}|}
      ~expected:None
  ; "field_opt decode error" >::
    fun _ ->
      match decode_string (field_opt "optional" string) {|{"optional": 123}|} with
      | Ok _ ->
        assert_string
          "expected decode error"
      | Error e ->
        assert_equal ~printer:CCFun.id
          {|in field "optional": Expected a string, but got 123.|}
          (Format.asprintf "%a" pp_error e)
  ]

let ezjsonm_encoders_suite =
  let open Decoders_ezjsonm.Encode in
  "Ezjsonm encoders" >:::
  [ "list string" >::
    (fun _ctxt ->
       assert_equal ~printer:CCFun.id
         {|["hello","world"]|}
         (encode_string (list string) ["hello"; "world"])
    )
  ; "string" >::
    (fun _ctxt ->
       assert_equal ~printer:CCFun.id
         {|"hello"|}
         (encode_string string "hello")
    )
  ]

let () =
  "decoders" >:::
  [ ezjsonm_suite
  ; ezjsonm_encoders_suite
  ]
  |> run_test_tt_main
